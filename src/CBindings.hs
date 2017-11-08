
module CBindings where

import Control.Monad
import Control.Concurrent
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array

data CLoopnaut

data CBindings = CBindings {
  create_loopnaut :: IO (Ptr CLoopnaut),
  set_buffer :: Ptr CLoopnaut -> Ptr CFloat -> Int -> IO ()
}
