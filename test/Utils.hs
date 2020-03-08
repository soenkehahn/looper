module Utils where

import Control.Concurrent
import Data.List (foldl')
import Data.Map
import Data.IORef
import Data.Traversable
import Development.Shake
import Foreign.C.Types
import Foreign.Marshal.Array
import Loopnaut
import Loopnaut.CBindings
import Loopnaut.Cli
import Loopnaut.FileWatcher
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
  let cliArgs = CliArgs file watched
  (mockFileWatcher, mockFileSystem) <- mkMockFileWatcher
  withRun bindings mockFileWatcher cliArgs $ do
    test mockFileSystem

mkMockFileWatcher :: IO (FileWatcher, MockFileSystem)
mkMockFileWatcher = do
  handlers <- newMVar empty
  let fileWatcher = FileWatcher {
        watchFiles = \ files handle action -> do
          modifyMVar_ handlers $ \ map -> do
            return $ Data.List.foldl'
              (\ acc file -> insert file handle acc)
              map
              files
          action
      }
      mockFileSystem = MockFileSystem handlers
  return (fileWatcher, mockFileSystem)

data MockFileSystem = MockFileSystem (MVar (Map FilePath (FilePath -> IO ())))

triggerHandlers :: MockFileSystem -> FilePath -> IO ()
triggerHandlers (MockFileSystem handlers) file =
  withMVar handlers $ \ map -> do
    case map !? file of
      Just h -> h file
      Nothing -> return ()

cp :: MockFileSystem -> FilePath -> FilePath -> IO ()
cp mockFileSystem source target = do
  unit $ cmd "cp" source target
  triggerHandlers mockFileSystem target

rm :: MockFileSystem -> FilePath -> IO ()
rm _ file = do
  unit $ cmd "rm" file

write :: MockFileSystem -> FilePath -> String -> IO ()
write mockFileSystem file contents = do
  writeFile file contents
  triggerHandlers mockFileSystem file
