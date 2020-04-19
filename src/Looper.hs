{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Looper (
  setBuffer,
  run,
  withRun,

  -- exported for testing
  _warnAboutInvalidSamples,
) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Looper.CBindings
import Looper.Cli
import Looper.File.Executable
import Looper.File.SndFile
import Looper.FileWatcher.Common
import System.Directory
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vec
import System.IO

convertToCArray :: Vector Double -> IO (Ptr CFloat, Int)
convertToCArray vector = do
  array <- mallocArray $ Vec.length vector
  let inner :: Int -> Double -> IO Int
      inner i sample = do
        pokeElemOff array i (realToFrac sample)
        return (i + 1)
  Vec.foldM'_ inner 0 vector
  return (array, Vec.length vector)

setBuffer :: CBindings -> Ptr CLooper -> Vector Double -> IO ()
setBuffer bindings looper list = do
  (array, len) <- convertToCArray list
  set_buffer bindings looper array len

run :: CBindings -> FileWatcher -> CliArgs -> IO ()
run bindings fileWatcher cliArgs = case cliArgs of
  Loop file watched -> withRun bindings fileWatcher file watched $ do
    forever $ threadDelay 1000000
  Render file outputFile -> do
    buffer <- readFromFile file
    _warnAboutInvalidSamples buffer
    writeToSndFile outputFile buffer

withRun :: CBindings -> FileWatcher -> FilePath -> [FilePath] -> IO a -> IO a
withRun bindings fileWatcher file watched action = do
  looper <- create_looper bindings
  updateLooper bindings looper file file
  watchFiles fileWatcher (file : watched)
    (\ changedFile -> updateLooper bindings looper file changedFile)
    action

updateLooper :: CBindings -> Ptr CLooper -> FilePath -> FilePath -> IO ()
updateLooper bindings looper file changedFile = catchExceptions $ do
  exists <- doesFileExist file
  when (not exists) $ do
    throwIO $ ErrorCall ("file not found: " ++ file)
  buffer <- addLogging file changedFile $ readFromFile file
  _warnAboutInvalidSamples buffer
  setBuffer bindings looper buffer

addLogging :: FilePath -> FilePath -> IO a -> IO a
addLogging file changedFile action = do
  hPutStrLn stderr $
    (if file /= changedFile then changedFile ++ " changed, " else "") ++
    "reading audio snippet from " ++ file ++ "..."
  hFlush stderr
  result <- action
  hPutStrLn stderr "done"
  return result

catchExceptions :: IO () -> IO ()
catchExceptions action = catch action $ \ (exception :: SomeException) -> do
  hPutStrLn stderr (show exception)

readFromFile :: FilePath -> IO (Vector Double)
readFromFile file = do
  fromExecutable <- readFromExecutable file
  case fromExecutable of
    ExecutableSuccess result -> return result
    ExecutableDecodingError error -> throwIO $ ErrorCall error
    PermissionError -> do
      fromSndFile <- readFromSndFile file
      case fromSndFile of
        SndFileSuccess result -> return result
        SndFileError error ->
          throwIO $ ErrorCall $ intercalate "\n" $
            (file ++ " is neither an executable (the executable flag is not set)") :
            "nor is it a sound file:" :
            ("  " ++ error) :
            []

_warnAboutInvalidSamples :: Vector Double -> IO ()
_warnAboutInvalidSamples vector =
  when (Vec.length vector > 0) $ do
    let max = Vec.maximum vector
        min = Vec.minimum vector
    when (min < -1 || max > 1) $ do
      hPutStrLn stderr $
        "warning: some audio samples are outside the valid range:\n" ++
        "min: " ++ show min ++
        ", max: " ++ show max
