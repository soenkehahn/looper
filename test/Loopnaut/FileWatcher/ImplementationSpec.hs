module Loopnaut.FileWatcher.ImplementationSpec (spec) where

import Test.Hspec
import System.Directory
import Control.Concurrent
import Data.Function
import Control.Exception
import System.Timeout
import Test.Mockery.Directory
import Development.Shake
import Loopnaut.FileWatcher.Implementation

writeMVar :: MVar a -> a -> IO ()
writeMVar ref a = do
  _ <- swapMVar ref a
  return ()

modify :: MVar a -> (a -> a) -> IO ()
modify ref f = modifyMVar_ ref (\ old -> return (f old))

waitFor :: IO Bool -> IO ()
waitFor action = do
  result <- timeout 500000 $ fix $ \ loop -> do
    condition <- action
    if condition
      then return ()
      else do
        threadDelay 10000
        loop
  case result of
    Nothing -> throwIO $ ErrorCall "waitFor took too long"
    Just () -> return ()

spec :: Spec
spec = around_ inTempDirectory $ describe "fileWatcher" $ do
  it "executes the given action" $ do
    ref <- newMVar False
    register fileWatcher [] (\ _ -> return ()) $ do
      writeMVar ref True
    readMVar ref `shouldReturn` True

  it "executes the given handler on file changes" $ do
    writeFile "file" "foo"
    ref <- newMVar False
    register fileWatcher ["file"] (\ _ -> writeMVar ref True) $ do
      _ <- forkIO $ do
        threadDelay 50000
        writeFile "file" "bar"
      waitFor (readMVar ref)

  it "propagates errors from handlers" $ do
    writeFile "file" "foo"
    let handle _file = do
          throwIO $ ErrorCall "exception from handler"
    let action = register fileWatcher ["a"] handle $ do
          _ <- forkIO $ do
            threadDelay 50000
            writeFile "file" "bar"
          waitFor (return False)
    action `shouldThrow` errorCall "exception from handler"

  it "passes in the changed file to the handler" $ do
    writeFile "file" "foo"
    ref <- newMVar Nothing
    register fileWatcher ["file"] (writeMVar ref . Just) $ do
      _ <- forkIO $ do
        threadDelay 50000
        writeFile "file" "bar"
      waitFor ((Nothing /=) <$> readMVar ref)
    readMVar ref `shouldReturn` Just "file"

  it "works multiple times" $ do
    writeFile "file" "foo"
    ref <- newMVar (0 :: Integer)
    register fileWatcher ["file"] (\ _ -> modify ref (+ 1)) $ do
      _ <- forkIO $ do
        threadDelay 50000
        writeFile "file" "bar"
        threadDelay 50000
        writeFile "file" "baz"
      waitFor ((2 ==) <$> readMVar ref)

  it "allows to watch multiple files" $ do
    writeFile "a" "foo"
    writeFile "b" "foo"
    ref <- newMVar []
    register fileWatcher ["a", "b"] (\ file -> modify ref (++ [file])) $ do
      _ <- forkIO $ do
        threadDelay 50000
        writeFile "a" "bar"
        threadDelay 50000
        writeFile "b" "baz"
      waitFor ((["a", "b"] ==) <$> readMVar ref)

  describe "when first removing the file" $ do
    it "triggers once when recreating the file" $ do
      writeFile "file" "foo"
      ref <- newMVar []
      let handle file = do
            contents <- readFile file
            modify ref (++ [contents])
            appendFile "/tmp/foo" "handled\n"
      register fileWatcher ["file"] handle $ do
        _ <- forkIO $ do
          threadDelay 50000
          removeFile "file"
          threadDelay 50000
          writeFile "file" "bar"
        waitFor ((>= 1) . length <$> readMVar ref)
      readMVar ref `shouldReturn` ["bar"]

    it "triggers once when recreating the file by copying" $ do
      writeFile "source" "source-contents"
      writeFile "target" "target-contents"
      ref <- newMVar []
      let handle file = do
            contents <- readFile file
            modify ref (++ [contents])
      register fileWatcher ["target"] handle $ do
        _ <- forkIO $ do
          threadDelay 50000
          unit $ cmd "rm target"
          threadDelay 50000
          unit $ cmd "cp source target"
        waitFor ((>= 1) . length <$> readMVar ref)
      readMVar ref `shouldReturn` ["source-contents"]

  it "passes correct files into handlers for files in subdirectories" $ do
    unit $ cmd "mkdir dir"
    writeFile "dir/file" "foo"
    ref <- newMVar Nothing
    register fileWatcher ["dir/file"] (writeMVar ref . Just) $ do
      _ <- forkIO $ do
        threadDelay 50000
        writeFile "dir/file" "bar"
      waitFor ((Nothing /=) <$> readMVar ref)
    readMVar ref `shouldReturn` Just "dir/file"
