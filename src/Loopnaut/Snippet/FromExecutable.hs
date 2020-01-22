{-# LANGUAGE ScopedTypeVariables #-}

module Loopnaut.Snippet.FromExecutable where

import Control.DeepSeq
import Control.Exception
import Data.List
import Loopnaut.Cli
import System.Exit
import System.IO
import System.IO.Error
import System.Process
import Text.Read

data FromExecutable
  = ExecutableSuccess [Double]
  | PermissionError
  | ExecutableDecodingError String

readFromExecutable :: File -> IO FromExecutable
readFromExecutable file = do
  output <- catchPermissionErrors $ do
    (stdout, exitCode) <- readFromProcess $ canonicalPath file
    return (exitCode, stdout)
  return $ case output of
    Nothing -> PermissionError
    Just (ExitFailure exitCode, _) ->
      ExecutableDecodingError (renderFile file ++ " failed with exit code " ++ show exitCode)
    Just (ExitSuccess, output) -> case parseResult file output of
      Left error -> ExecutableDecodingError error
      Right samples -> ExecutableSuccess samples

catchPermissionErrors :: IO a -> IO (Maybe a)
catchPermissionErrors action =
  catch (Just <$> action) $ \ (e :: IOError) ->
    if isPermissionError e
      then return Nothing
      else throwIO e

readFromProcess :: FilePath -> IO (String, ExitCode)
readFromProcess file = do
  let createProcess = (proc file []){
    std_out = CreatePipe
  }
  withCreateProcess createProcess $ \ Nothing (Just stdoutHandle) Nothing processHandle -> do
    stdout <- hGetContents stdoutHandle
    deepseq stdout (return ())
    exitCode <- waitForProcess processHandle
    return (stdout, exitCode)

parseResult :: File -> String -> Either String [Double]
parseResult file = mapM parseLine . lines
  where
    parseLine :: String -> Either String Double
    parseLine line = case readMaybe line of
      Nothing -> Left $ intercalate "\n" $
        (renderFile file ++ " wrote a line to stdout that cannot be parsed as a number:") :
        line :
        []
      Just result -> return result
