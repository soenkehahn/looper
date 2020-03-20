{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Looper.Cli (
  CliArgs(..),
  withCliArgs,
) where

import Control.Exception
import Data.List
import WithCli

data CliArgs
  = Loop {
    cliArgsFile :: FilePath,
    cliArgsWatched :: [FilePath]
  }
  | Render {
    cliArgsFile :: FilePath,
    cliArgsOutputFile :: FilePath
  }
  deriving (Eq, Show, Generic)

data Inner = Inner {
  file :: [FilePath],
  watch :: [CliFile],
  render :: Maybe CliFile
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
withCliArgs action = do
  let modifiers = [UseForPositionalArguments "file" "SNIPPETFILE"]
  withCliModified modifiers $
    \ (Inner files (map path -> watched) (fmap path -> render)) -> case files of
      [file] -> case render of
        Nothing -> action (Loop file watched)
        Just render -> action (Render file render)
      [] -> throwIO $ ErrorCall $ "no main file given"
      _ : _ : _ -> throwIO $ ErrorCall $
        "multiple main files given: " ++
        intercalate ", " files ++
        "\nplease provide only one main file"
