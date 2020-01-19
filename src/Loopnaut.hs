{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Loopnaut where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import Data.String.Conversions
import Data.Void
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Loopnaut.CBindings
import Loopnaut.Cli
import Loopnaut.Snippet.FromExecutable
import Loopnaut.Snippet.FromSndFile
import System.Directory
import System.FilePath
import System.INotify
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

run :: CBindings -> CliArgs -> IO Void
run bindings cliArgs = do
  let CliArgs file watched = cliArgs
  loopnaut <- create bindings
  updateLoopnaut bindings loopnaut file file
  watchFiles (map dropFileName (file : watched)) $ \ changedFile -> do
    when (changedFile == file || changedFile `elem` watched) $ do
      updateLoopnaut bindings loopnaut file changedFile

watchFiles :: [FilePath] -> (FilePath -> IO ()) -> IO Void
watchFiles (nub -> dirs) action = do
  withINotify $ \ inotify -> do
    forM_ dirs $ \ dir -> do
      addWatch inotify [Close] (cs dir) $ \ event -> do
        case event of
          Closed{maybeFilePath = Just (cs -> changedFile), wasWriteable = True} -> do
            action $ normalise (dir </> changedFile)
          _ -> return ()
    forever $ threadDelay 1000000

updateLoopnaut :: CBindings -> Ptr CLoopnaut -> FilePath -> FilePath -> IO ()
updateLoopnaut bindings loopnaut file changedFile = do
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
