module Looper.FileWatcher (fileWatcher) where

import Control.Monad
import Data.List
import Data.String.Conversions
import Looper.FileWatcher.Common
import System.FilePath
import System.INotify

fileWatcher :: FileWatcher
fileWatcher = FileWatcher {
  watchFiles = \ watchedFiles handler action -> do
    let dirs = nub $ map dropFileName watchedFiles
    wrapCallback <- callbackExceptionPropagator
    withINotify $ \ inotify -> do
      forM_ dirs $ \ dir -> do
        addWatch inotify [Close] (cs dir) $ \ event -> wrapCallback $ do
          case event of
            Closed{maybeFilePath = Just changedFile, wasWriteable = True} -> do
              handleWhenWatched watchedFiles (dir </> cs changedFile) handler
            _ -> return ()
      action
}
