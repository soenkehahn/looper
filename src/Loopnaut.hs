{-# LANGUAGE ScopedTypeVariables #-}

module Loopnaut where

import Control.Monad
import Control.Concurrent
import CBindings
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

allocateList :: Storable a => [a] -> IO (Ptr a, Int)
allocateList list = do
  let len = length list
  array <- mallocArray len
  pokeArray array list
  return (array, len)

loopBuffer :: CBindings -> [CFloat] -> IO (Ptr Buffer)
loopBuffer bindings list = do
  (array, len) <- allocateList list
  loop_buffer bindings array len

setBuffer :: CBindings -> Ptr Buffer -> [CFloat] -> IO ()
setBuffer bindings buffer list = do
  (array, len) <- allocateList list
  set_buffer bindings buffer array len

main :: CBindings -> IO ()
main bindings = do
  buffer <- loopBuffer bindings $ take 100 [0, 0.01 ..]
  threadDelay 2000000
  setBuffer bindings buffer [0.4]
  forever $ threadDelay 1000000
