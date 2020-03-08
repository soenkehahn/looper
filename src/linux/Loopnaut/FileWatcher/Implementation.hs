{-# LANGUAGE RankNTypes #-}

module Loopnaut.FileWatcher.Implementation where

import System.INotify
import Data.String.Conversions
import Data.Foldable
import System.Directory
import System.FilePath
import Data.List

data FileWatcher = FileWatcher {
  register :: forall a . [FilePath] -> (FilePath -> IO ()) -> IO a -> IO a
}

fileWatcher :: FileWatcher
fileWatcher = FileWatcher $ \ files handler action -> do
  let dirs = nub $ map dropFileName files
  withINotify $ \ inotify -> do
    forM_ dirs $ \ dir -> do
      addWatch inotify [Close] (cs dir) $ \ event -> do
        case event of
          Closed{maybeFilePath = Just changed, wasWriteable = True} -> do
            changedFile <- canonicalizePath (dir </> cs changed)
            handler changedFile
          _ -> return ()
    action
