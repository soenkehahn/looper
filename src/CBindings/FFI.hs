
module CBindings.FFI where

import CBindings
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "create_loopnaut" c_create_loopnaut :: IO (Ptr CLoopnaut)

foreign import ccall "set_buffer" c_set_buffer :: Ptr CLoopnaut -> Ptr CFloat -> Int -> IO ()

cBindings :: CBindings
cBindings = CBindings {
  create_loopnaut = c_create_loopnaut,
  set_buffer = c_set_buffer
}
