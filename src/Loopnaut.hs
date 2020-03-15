{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Loopnaut where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Loopnaut.CBindings
import Loopnaut.Cli
import Loopnaut.FileWatcher.Common
import Loopnaut.Snippet.FromExecutable
import Loopnaut.Snippet.FromSndFile
import System.Directory
import System.IO

create :: CBindings -> IO (Ptr CLoopnaut)
create = create_loopnaut

allocateList :: Storable a => [a] -> IO (Ptr a, Int)
allocateList list = do
  let len = length list
  array <- mallocArray len
  pokeArray array list
  return (array, len)

setBuffer :: CBindings -> Ptr CLoopnaut -> [CFloat] -> IO ()
setBuffer bindings loopnaut list = do
  (array, len) <- allocateList list
  set_buffer bindings loopnaut array len

run :: CBindings -> FileWatcher -> CliArgs -> IO ()
run bindings fileWatcher cliArgs = case cliArgs of
  Loop file watched -> withRun bindings fileWatcher file watched $ do
    forever $ threadDelay 1000000
  Render file outputFile -> do
    buffer <- tryReaders file (readFromExecutable file) (readFromSndfile file)
    writeToSndfile outputFile buffer

withRun :: CBindings -> FileWatcher -> FilePath -> [FilePath] -> IO a -> IO a
withRun bindings fileWatcher file watched action = do
  loopnaut <- create bindings
  updateLoopnaut bindings loopnaut file file
  watchFiles fileWatcher (file : watched)
    (\ changedFile -> updateLoopnaut bindings loopnaut file changedFile)
    action

updateLoopnaut :: CBindings -> Ptr CLoopnaut -> FilePath -> FilePath -> IO ()
updateLoopnaut bindings loopnaut file changedFile = catchExceptions $ do
  hPutStr stderr $
    (if file /= changedFile then changedFile ++ " changed, " else "") ++
    "reading audio snippet from " ++ file ++ "...\n"
  hFlush stderr
  exists <- doesFileExist file
  when (not exists) $ do
    throwIO $ ErrorCall ("file not found: " ++ file)
  buffer <- tryReaders file
    (readFromExecutable file)
    (readFromSndfile file)
  hPutStrLn stderr "done"
  setBuffer bindings loopnaut (map realToFrac buffer)

catchExceptions :: IO () -> IO ()
catchExceptions action = catch action $ \ (exception :: SomeException) -> do
  hPutStrLn stderr (show exception)

tryReaders :: FilePath -> IO FromExecutable -> IO FromSndfile -> IO [Double]
tryReaders file readFromExecutable readFromSndFile = do
  fromExecutable <- readFromExecutable
  case fromExecutable of
    ExecutableSuccess result -> return result
    ExecutableDecodingError error -> throwIO $ ErrorCall error
    PermissionError -> do
      fromSndFile <- readFromSndFile
      case fromSndFile of
        SndFileSuccess result -> return result
        SndFileError error ->
          throwIO $ ErrorCall $ intercalate "\n" $
            (file ++ " is neither an executable (the executable flag is not set)") :
            "nor is it a sound file:" :
            ("  " ++ error) :
            []
