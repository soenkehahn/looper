module Loopnaut.FileWatcher (fileWatcher) where

import Control.Monad
import Data.List
import Data.String.Conversions
import Loopnaut.FileWatcher.Common
import System.FilePath
import System.INotify

fileWatcher :: FileWatcher
fileWatcher = FileWatcher {
  watchFiles = \ files handler action -> do
    let dirs = nub $ map dropFileName files
    wrapCallback <- callbackExceptionPropagator
    withINotify $ \ inotify -> do
      forM_ dirs $ \ dir -> do
        addWatch inotify [Close] (cs dir) $ \ event -> wrapCallback $ do
          case event of
            Closed{maybeFilePath = Just file, wasWriteable = True} -> do
              let changed = dir </> cs file
              triggers <- filterM (=== changed) files
              case triggers of
                trigger : _ -> handler trigger
                [] -> return ()
            _ -> return ()
      action
}
