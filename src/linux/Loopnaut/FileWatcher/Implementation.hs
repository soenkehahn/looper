{-# LANGUAGE RankNTypes #-}

module Loopnaut.FileWatcher.Implementation where

import Data.Foldable
import Data.List
import Data.String.Conversions
import System.FilePath
import System.INotify

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
            handler (dir </> cs changed)
          _ -> return ()
    action
