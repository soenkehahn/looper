{-# LANGUAGE RankNTypes #-}

module Loopnaut.FileWatcher.Implementation where

import Data.Foldable
import Control.Exception
import Data.List
import Data.String.Conversions
import System.FilePath
import System.INotify
import Control.Concurrent

data FileWatcher = FileWatcher {
  register :: forall a . [FilePath] -> (FilePath -> IO ()) -> IO a -> IO a
}

fileWatcher :: FileWatcher
fileWatcher = FileWatcher $ \ files handler action -> do
  let dirs = nub $ map dropFileName files
  exceptionChannel <- newChan
  withINotify $ \ inotify -> do
    forM_ dirs $ \ dir -> do
      addWatch inotify [Close] (cs dir) $ \ event -> do
        handle (writeChan exceptionChannel :: SomeException -> IO ()) $ do
          case event of
            Closed{maybeFilePath = Just changed, wasWriteable = True} -> do
              handler (normalise (dir </> cs changed))
            _ -> return ()
    outerThread <- myThreadId
    _ <- forkIO $ do
      exception <- readChan exceptionChannel
      throwTo outerThread exception
    action
