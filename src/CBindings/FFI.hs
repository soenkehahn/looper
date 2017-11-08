
module CBindings.FFI where

import CBindings
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "loop_buffer" c_loop_buffer :: Ptr CFloat -> Int -> IO (Ptr Buffer)

foreign import ccall "set_buffer" c_set_buffer :: Ptr Buffer -> Ptr CFloat -> Int -> IO ()

cBindings :: CBindings
cBindings = CBindings {
  loop_buffer = c_loop_buffer,
  set_buffer = c_set_buffer
}
