{-# LANGUAGE RankNTypes #-}

module Loopnaut.FileWatcher.Common where

import Control.Concurrent
import Control.Exception
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

(===) :: FilePath -> FilePath -> IO Bool
a === b = do
  canonicalA <- canonicalizePath a
  canonicalB <- canonicalizePath b
  return (canonicalA == canonicalB)
