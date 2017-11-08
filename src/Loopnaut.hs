{-# LANGUAGE ScopedTypeVariables #-}

module Loopnaut where

import Control.Monad
import Control.Concurrent
import CBindings
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

create :: CBindings -> IO (Ptr CLoopnaut)
create = create_loopnaut

allocateList :: Storable a => [a] -> IO (Ptr a, Int)
allocateList list = do
  let len = length list
  array <- mallocArray len
  pokeArray array list
  return (array, len)

setBuffer :: CBindings -> Ptr CLoopnaut -> [CFloat] -> IO ()
setBuffer bindings buffer list = do
  (array, len) <- allocateList list
  set_buffer bindings buffer array len

main :: CBindings -> IO ()
main bindings = do
  buffer <- create bindings
  setBuffer bindings buffer $ take 100 [0, 0.01 ..]
  threadDelay 1000000
  setBuffer bindings buffer [0.4]
  threadDelay 1000000
