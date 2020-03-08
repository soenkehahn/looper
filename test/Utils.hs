module Utils where

import Control.Concurrent
import Data.IORef
import Data.Traversable
import Development.Shake
import Foreign.C.Types
import Foreign.Marshal.Array
import Loopnaut
import Loopnaut.CBindings
import Loopnaut.Cli
import Loopnaut.FileWatcher.Implementation
import System.Timeout

withMockBindings :: (CBindings -> IO ()) -> IO [([CFloat], Int)]
withMockBindings action = do
  mockBuffer <- newIORef []
  let mockBindings = CBindings {
    create_loopnaut = do
      return (error "mock Ptr Buffer"),
    set_buffer = \ _ buffer len -> do
      modifyIORef mockBuffer (\ acc -> acc ++ [(buffer, len)])
  }
  action mockBindings
  setBuffers <- readIORef mockBuffer
  forM setBuffers $ \ (array, len) -> do
    peeked <- peekArray len array
    return (peeked, len)

timebox :: IO a -> IO ()
timebox action = do
  _ <- timeout 400000 action
  return ()

testRun :: CBindings -> FilePath -> [FilePath] -> (MockFileSystem -> IO a) -> IO a
testRun bindings file watched test = do
  cliArgs <- CliArgs <$> mkFile file <*> mapM mkFile watched
  (mockFileWatcher, mockFileSystem) <- mkMockFileWatcher
  withRun bindings mockFileWatcher cliArgs $ do
    test mockFileSystem

mkMockFileWatcher :: IO (FileWatcher, MockFileSystem)
mkMockFileWatcher = do
  handlerRef <- newMVar (\ _ -> return ())
  let fileWatcher = FileWatcher {
        register = \ _files handler action -> do
          _ <- swapMVar handlerRef handler
          action
      }
      mockFileSystem = MockFileSystem handlerRef
  return (fileWatcher, mockFileSystem)

data MockFileSystem = MockFileSystem (MVar (FilePath -> IO ()))

cp :: MockFileSystem -> FilePath -> FilePath -> IO ()
cp (MockFileSystem handlerRef) source target = do
  withMVar handlerRef $ \ handler -> do
    unit $ cmd "cp" source target
    handler target

rm :: MockFileSystem -> FilePath -> IO ()
rm (MockFileSystem handlerRef) file = do
  withMVar handlerRef $ \ _handler -> do
    unit $ cmd "rm" file

write :: MockFileSystem -> FilePath -> String -> IO ()
write (MockFileSystem handlerRef) file contents = do
  withMVar handlerRef $ \ handler -> do
    writeFile file contents
    handler file
