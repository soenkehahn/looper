{-# LANGUAGE ScopedTypeVariables #-}

module Loopnaut where

import Control.Monad
import Control.Concurrent
import CBindings
import Foreign.C.Types
import Foreign.Marshal.Array

loopBuffer :: CBindings -> [CFloat] -> IO ()
loopBuffer bindings buffer = do
  let len = length buffer
  array <- mallocArray len
  pokeArray array buffer
  loop_buffer bindings array len

main :: IO ()
main = do
  loopBuffer cBindings $ take 100 [0, 0.01 ..]
  forever $ threadDelay 1000000
