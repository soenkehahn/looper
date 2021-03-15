{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Looper.File.Executable (
  FromExecutable(..),
  readFromExecutable,

  -- exported for testing
  _parse,
) where

import Control.Exception
import Control.Monad
import Data.ByteString.Conversion
import Data.IORef
import Data.String.Conversions
import Data.Vector.Storable (Vector)
import Looper.File.SndFile (samplerate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Vector.Storable as Vec
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Process

data FromExecutable
  = ExecutableSuccess (Vector Double)
  | PermissionError
  | ExecutableDecodingError String

readFromExecutable :: FilePath -> IO FromExecutable
readFromExecutable file = do
  output <- catchPermissionErrors $ do
    readFromProcess ("." </> file)
  return $ case output of
    Nothing -> PermissionError
    Just (ExitFailure exitCode, _) ->
      ExecutableDecodingError (file ++ " failed with exit code " ++ show exitCode)
    Just (ExitSuccess, output) ->
      either ExecutableDecodingError ExecutableSuccess output

catchPermissionErrors :: IO a -> IO (Maybe a)
catchPermissionErrors action =
  catch (Just <$> action) $ \ (e :: IOError) ->
    if isPermissionError e
      then return Nothing
      else throwIO e

readFromProcess :: FilePath -> IO (ExitCode, Either String (Vector Double))
readFromProcess file = do
  let createProcess = (proc file []){
    std_out = CreatePipe
  }
  withCreateProcess createProcess $ \ Nothing (Just stdoutHandle) Nothing processHandle -> do
    string <- BS.hGetContents stdoutHandle
    exitCode <- waitForProcess processHandle
    vector <- _parse file string
    return (exitCode, vector)

_parse :: FilePath -> BS.ByteString -> IO (Either String (Vector Double))
_parse file string = do
  withProgressReporter $ \ progressReporter -> do
    errorRef <- newIORef Nothing
    let inner string = do
          let (line, BS.drop 1 -> rest) = Char8.break (== '\n') string
          case fromByteString line :: Maybe Double of
            Nothing -> do
              writeIORef errorRef $ Just $
                file ++ " wrote a line to stdout that cannot be parsed as a number:\n" ++ cs line
              return Nothing
            Just sample -> do
              count progressReporter
              return $ Just (sample, rest)
    vector <- Vec.unfoldrNM (Char8.count '\n' string) inner string
    error <- readIORef errorRef
    return $ case error of
      Just error -> Left error
      Nothing -> Right vector

data ProgressReporter
  = ProgressReporter (IORef Int)

withProgressReporter :: (ProgressReporter -> IO a) -> IO a
withProgressReporter action = do
  bracket
    (ProgressReporter <$> newIORef 0)
    (\ (ProgressReporter indexRef) -> do
      index <- readIORef indexRef
      when (index >= limit) $
        hPutStr stderr "\n")
    action

limit :: Int
limit = round (fromIntegral samplerate / 10 :: Double)

count :: ProgressReporter -> IO ()
count (ProgressReporter indexRef) = do
  index <- (+ 1) <$> readIORef indexRef
  writeIORef indexRef index
  when (index `mod` limit == 0) $ do
    let seconds :: Double
        seconds = fromIntegral index / fromIntegral samplerate
        carriageReturn = if index == limit then "" else "\r"
    hPutStr stderr (carriageReturn ++ "read " ++ show seconds ++ " seconds")
