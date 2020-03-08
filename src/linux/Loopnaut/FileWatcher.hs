{-# LANGUAGE RankNTypes #-}

module Loopnaut.FileWatcher where

import Data.Foldable
import System.Directory
import Control.Monad
import Control.Exception
import Data.List
import Data.String.Conversions
import System.FilePath
import System.INotify
import Control.Concurrent

data FileWatcher = FileWatcher {
  watchFiles :: forall a . [FilePath] -> (FilePath -> IO ()) -> IO a -> IO a
}

fileWatcher :: FileWatcher
fileWatcher = FileWatcher {
  watchFiles = \ files handler action -> do
    let dirs = nub $ map dropFileName files
    exceptionChannel <- newChan
    withINotify $ \ inotify -> do
      forM_ dirs $ \ dir -> do
        addWatch inotify [Close] (cs dir) $ \ event -> do
          handle (writeChan exceptionChannel :: SomeException -> IO ()) $ do
            case event of
              Closed{maybeFilePath = Just file, wasWriteable = True} -> do
                let changed = dir </> cs file
                triggers <- filterM (=== changed) files
                case triggers of
                  trigger : _ -> handler trigger
                  [] -> return ()
              _ -> return ()
      outerThread <- myThreadId
      _ <- forkIO $ do
        exception <- readChan exceptionChannel
        throwTo outerThread exception
      action
}

(===) :: FilePath -> FilePath -> IO Bool
a === b = do
  canonicalA <- canonicalizePath a
  canonicalB <- canonicalizePath b
  return (canonicalA == canonicalB)
