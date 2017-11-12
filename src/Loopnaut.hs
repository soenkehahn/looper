{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Loopnaut where

import CBindings
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Sound.File.Sndfile as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as BV
import Data.Vector.Storable (toList)
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

data CliArgs = CliArgs FilePath
  deriving (Show, Generic)

instance HasArguments CliArgs

run :: CBindings -> CliArgs -> IO ()
run bindings cliArgs = do
  let CliArgs file = cliArgs
  (_info, mBuffer :: Maybe (BV.Buffer Double)) <- Snd.readFile file
  case mBuffer of
    Just fileContent -> do
      loopnaut <- create bindings
      let sampleList =
            map realToFrac (toList (BV.fromBuffer fileContent))
      setBuffer bindings loopnaut sampleList
    Nothing -> do
      error "file empty"
