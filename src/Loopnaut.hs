{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Loopnaut where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import Data.String.Conversions
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

run :: CBindings -> CliArgs -> IO a
run bindings cliArgs = withRun bindings cliArgs $ do
  forever $ threadDelay 1000000

withRun :: CBindings -> CliArgs -> IO a -> IO a
withRun bindings cliArgs action = do
  let CliArgs file watched = cliArgs
  loopnaut <- create bindings
  updateLoopnaut bindings loopnaut file file
  watchFiles (file : watched)
    (\ changedFile -> updateLoopnaut bindings loopnaut file changedFile)
    action

watchFiles :: [File] -> (File -> IO ()) -> IO a -> IO a
watchFiles files handler action = do
  let dirs = nub $ map (dropFileName . canonicalPath) files
  withINotify $ \ inotify -> do
    forM_ dirs $ \ dir -> do
      addWatch inotify [Close] (cs dir) $ \ event -> do
        case event of
          Closed{maybeFilePath = Just changed, wasWriteable = True} -> do
            changedFile <- canonicalizePath (dir </> cs changed)
            let triggers = filter (\ w -> canonicalPath w == changedFile) files
            case triggers of
              trigger : _ -> do
                handler trigger
              [] -> return ()
          _ -> return ()
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
