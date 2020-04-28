{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Looper.Cli (
  CliArgs(..),
  Normalization(..),
  withCliArgs,
) where

import Control.Exception
import Data.List
import WithCli

data CliArgs
  = Loop {
    cliArgsFile :: FilePath,
    cliArgsWatched :: [FilePath],
    cliArgsNormalization :: Normalization
  }
  | Render {
    cliArgsFile :: FilePath,
    cliArgsOutputFile :: FilePath,
    cliArgsNormalization :: Normalization
  }
  deriving (Eq, Show, Generic)

data Normalization
  = DontNormalize
  | Normalize
  deriving (Eq, Show)

data Inner = Inner {
  file :: [FilePath],
  watch :: [CliFile],
  render :: Maybe CliFile,
  normalize :: Bool
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
  let modifiers =
        UseForPositionalArguments "file" "SNIPPETFILE" :
        AddShortOption "normalize" 'n' :
        []
  withCliModified modifiers $
    \ (Inner files (map path -> watched) (fmap path -> render) normalizeFlag) -> do
      let normalize = if normalizeFlag then Normalize else DontNormalize
      case files of
        [file] -> case render of
          Nothing -> action (Loop file watched normalize)
          Just render -> action (Render file render normalize)
        [] -> throwIO $ ErrorCall $ "no main file given"
        _ : _ : _ -> throwIO $ ErrorCall $
          "multiple main files given: " ++
          intercalate ", " files ++
          "\nplease provide only one main file"
