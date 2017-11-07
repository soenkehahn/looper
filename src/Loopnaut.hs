{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Concurrent
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import CBindings

main :: IO ()
main = do
  let len = 90
  array <- mallocArray len
  pokeArray array $ take len [0, 1 / 90 ..]
  loop_buffer cBindings array len
  forever $ threadDelay 1000000
