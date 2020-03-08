{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Loopnaut.Cli (
  CliArgs(..),
  withCliArgs,
) where

import Control.Exception
import Data.List
import WithCli

data CliArgs = CliArgs {
  cliArgsFile :: FilePath,
  cliArgsWatched :: [FilePath]
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
      [file] -> action (CliArgs file watched)
      [] -> throwIO $ ErrorCall $ "no main file given"
      _ : _ : _ -> throwIO $ ErrorCall $
        "multiple main files given: " ++
        intercalate ", " files ++
        "\nplease provide only one main file"
