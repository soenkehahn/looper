
module CBindings (CBindings(..), cBindings) where

import Control.Monad
import Control.Concurrent
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array

data CBindings = CBindings {
  loop_buffer :: Ptr CFloat -> Int -> IO ()
}

cBindings :: CBindings
cBindings = CBindings {
  loop_buffer = c_loop_buffer
}

foreign import ccall "loop_buffer" c_loop_buffer :: Ptr CFloat -> Int -> IO ()
