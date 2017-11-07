{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Concurrent
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array

main :: IO ()
main = do
  let len = 90
  array <- mallocArray len
  pokeArray array $ take len [0, 1 / 90 ..]
  c_loop_buffer array len
  forever $ threadDelay 1000000

foreign import ccall "loop_buffer" c_loop_buffer :: Ptr CFloat -> Int -> IO ()
