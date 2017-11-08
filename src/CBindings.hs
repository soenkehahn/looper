
module CBindings where

import Control.Monad
import Control.Concurrent
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array

data Buffer

data CBindings = CBindings {
  loop_buffer :: Ptr CFloat -> Int -> IO (Ptr Buffer),
  set_buffer :: Ptr Buffer -> Ptr CFloat -> Int -> IO ()
}
