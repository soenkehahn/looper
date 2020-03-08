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
import Loopnaut.FileWatcher
import Loopnaut.FileWatcher.Implementation
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

run :: CBindings -> FileWatcher -> CliArgs -> IO a
run bindings fileWatcher cliArgs = withRun bindings fileWatcher cliArgs $ do
  forever $ threadDelay 1000000

withRun :: CBindings -> FileWatcher -> CliArgs -> IO a -> IO a
withRun bindings fileWatcher cliArgs action = do
  let CliArgs file watched = cliArgs
  loopnaut <- create bindings
  updateLoopnaut bindings loopnaut file file
  watchFiles fileWatcher (file : watched)
    (\ changedFile -> updateLoopnaut bindings loopnaut file changedFile)
    action

updateLoopnaut :: CBindings -> Ptr CLoopnaut -> File -> File -> IO ()
updateLoopnaut bindings loopnaut file changedFile = do
  hPutStr stderr $
    (if file /= changedFile then renderFile changedFile ++ " changed, " else "") ++
    "reading audio snippet from " ++ renderFile file ++ "...\n"
  hFlush stderr
  exists <- doesFileExist $ canonicalPath file
  when (not exists) $ do
    throwIO $ ErrorCall ("file not found: " ++ renderFile file)
  buffer <- tryReaders file
    (readFromExecutable file)
    (readFromSndfile file)
  hPutStrLn stderr "done"
  setBuffer bindings loopnaut (map realToFrac buffer)

tryReaders :: File -> IO FromExecutable -> IO FromSndfile -> IO [Double]
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
            (renderFile file ++ " is neither an executable (the executable flag is not set)") :
            "nor is it a sound file:" :
            ("  " ++ error) :
            []
