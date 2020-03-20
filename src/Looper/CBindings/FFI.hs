module Looper.CBindings.FFI where

import Foreign.C.Types
import Foreign.Ptr
import Looper.CBindings

foreign import ccall "create_looper" c_create_looper ::
               IO (Ptr CLooper)

foreign import ccall "set_buffer" c_set_buffer ::
               Ptr CLooper -> Ptr CFloat -> Int -> IO ()

cBindings :: CBindings
cBindings =
  CBindings {create_looper = c_create_looper, set_buffer = c_set_buffer}
