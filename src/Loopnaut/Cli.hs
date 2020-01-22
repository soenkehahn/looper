{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Loopnaut.Cli (
  CliArgs(..),
  withCliArgs,
  File(canonicalPath, renderFile),
  mkFile,
) where

import Control.Exception
import Data.List
import System.Directory
import WithCli

data CliArgs = CliArgs {
  cliArgsFile :: File,
  cliArgsWatched :: [File]
} deriving (Eq, Show, Generic)

data Inner = Inner {
  file :: [FilePath],
  watch :: [CliFile]
} deriving (Show, Generic)

instance HasArguments Inner

data CliFile = CliFile {
  path :: FilePath
} deriving (Show, Eq)

instance HasArguments CliFile where
  argumentsParser = atomicArgumentsParser

instance Argument CliFile where
  argumentType Proxy = "FILE"
  parseArgument f = Just (CliFile f)

withCliArgs :: (CliArgs -> IO ()) -> IO ()
withCliArgs action =
  withCliModified [UseForPositionalArguments "file" "SNIPPETFILE"] $
    \ (Inner files (map path -> watched)) -> case files of
      [file] -> do
        cliArgs <- CliArgs <$> mkFile file <*> mapM mkFile watched
        action cliArgs
      [] -> throwIO $ ErrorCall $ "no main file given"
      _ : _ : _ -> throwIO $ ErrorCall $
        "multiple main files given: " ++
        intercalate ", " files ++
        "\nplease provide only one main file"

data File = File {
  renderFile :: FilePath,
  canonicalPath :: FilePath
} deriving (Show, Eq)

mkFile :: FilePath -> IO File
mkFile file = File file <$> canonicalizePath file
