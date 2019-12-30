{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Loopnaut where

import CBindings
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
import Loopnaut.FromExecutable
import Loopnaut.FromSndFile
import System.Directory
import System.FilePath
import System.INotify
import System.IO
import WithCli

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

data CliArgs = CliArgs File
  deriving (Show, Generic)

instance HasArguments CliArgs

data File = File FilePath
  deriving (Show)

instance HasArguments File where
  argumentsParser = atomicArgumentsParser

instance Argument File where
  argumentType Proxy = "SOUNDFILE"
  parseArgument f = Just (File f)

run :: CBindings -> CliArgs -> IO Void
run bindings cliArgs = do
  let CliArgs (File file) = cliArgs
  loopnaut <- create bindings
  updateLoopnaut bindings loopnaut file

  withINotify $ \ inotify -> do
    let dir = dropFileName file
    _ <- addWatch inotify [Close] (cs dir) $ \ event -> case event of
      Closed{maybeFilePath = Just (cs -> file), wasWriteable = True}
        | normalise (dir </> file) == normalise file
        -> updateLoopnaut bindings loopnaut file
      _ -> return ()
    forever $ threadDelay 1000000

updateLoopnaut :: CBindings -> Ptr CLoopnaut -> FilePath -> IO ()
updateLoopnaut bindings loopnaut file = do
  hPutStr stderr ("reading audio snippet from " ++ file ++ "...")
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
