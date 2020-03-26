{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Looper.File.Executable (
  FromExecutable(..),
  readFromExecutable,
) where

import Control.Exception
import Data.ByteString.Conversion
import Data.IORef
import Data.String.Conversions
import Data.Vector.Storable (Vector)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Vector.Storable as Vec
import System.Exit
import System.FilePath
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
    vector <- parse file string
    return (exitCode, vector)

parse :: FilePath -> BS.ByteString -> IO (Either String (Vector Double))
parse file string = do
  errorRef <- newIORef Nothing
  let inner string = do
        let (line, BS.drop 1 -> rest) = Char8.break (== '\n') string
        case fromByteString line :: Maybe Double of
          Nothing -> do
            writeIORef errorRef $ Just $
              file ++ " wrote a line to stdout that cannot be parsed as a number:\n" ++ cs line
            return Nothing
          Just sample -> return $ Just (sample, rest)
  vector <- Vec.unfoldrNM (Char8.count '\n' string) inner string
  error <- readIORef errorRef
  return $ case error of
    Just error -> Left error
    Nothing -> Right vector
