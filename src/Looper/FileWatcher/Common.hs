{-# LANGUAGE RankNTypes #-}

module Looper.FileWatcher.Common where

import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Directory

data FileWatcher = FileWatcher {
  watchFiles :: forall a . [FilePath] -> (FilePath -> IO ()) -> IO a -> IO a
}

callbackExceptionPropagator :: IO (IO () -> IO ())
callbackExceptionPropagator = do
  exceptionChannel <- newChan
  outerThread <- myThreadId
  _ <- forkIO $ do
    exception <- readChan exceptionChannel
    throwTo outerThread exception
  return (handle (writeChan exceptionChannel :: SomeException -> IO ()))

handleWhenWatched :: [FilePath] -> FilePath -> (FilePath -> IO ()) -> IO ()
handleWhenWatched watchedFiles changedFile handle = do
  triggers <- filterM (=== changedFile) watchedFiles
  case triggers of
    trigger : _ -> handle trigger
    [] -> return ()

(===) :: FilePath -> FilePath -> IO Bool
a === b = do
  canonicalA <- canonicalizePath a
  canonicalB <- canonicalizePath b
  return (canonicalA == canonicalB)
