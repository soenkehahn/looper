{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Loopnaut where

import CBindings
import Control.Concurrent
import Control.Monad
import Foreign.C.Types
import Foreign.Marshal.Array
import Data.Void
import Foreign.Ptr
import Data.String.Conversions
import Foreign.Storable
import Sound.File.Sndfile as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as BV
import Data.Vector.Storable (toList)
import WithCli
import System.INotify
import System.FilePath

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
  (_info, mBuffer :: Maybe (BV.Buffer Double)) <- Snd.readFile file
  case mBuffer of
    Just fileContent -> do
      let sampleList = map realToFrac (toList (BV.fromBuffer fileContent))
      setBuffer bindings loopnaut sampleList
    Nothing -> do
      error "file empty"
