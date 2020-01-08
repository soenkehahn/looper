{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Cli (
  CliArgs(..),
  withCliArgs,
) where

import Data.List
import System.FilePath
import Control.Exception
import WithCli

data CliArgs = CliArgs {
  cliArgsFile :: FilePath,
  cliArgsWatched :: [FilePath]
} deriving (Eq, Show, Generic)

data Inner = Inner {
  file :: [FilePath],
  watch :: [File]
} deriving (Show, Generic)

instance HasArguments Inner

data File = File {
  path :: FilePath
} deriving (Show, Eq)

instance HasArguments File where
  argumentsParser = atomicArgumentsParser

instance Argument File where
  argumentType Proxy = "FILE"
  parseArgument f = Just (File f)

withCliArgs :: (CliArgs -> IO ()) -> IO ()
withCliArgs action =
  withCliModified [UseForPositionalArguments "file" "SNIPPETFILE"] $
    \ (Inner files (map path -> watched)) -> case files of
      [file] -> action $ CliArgs (normalise file) (map normalise watched)
      [] -> throwIO $ ErrorCall $ "no main file given"
      _ : _ : _ -> throwIO $ ErrorCall $
        "multiple main files given: " ++
        intercalate ", " files ++
        "\nplease provide only one main file"
