module Loopnaut.CBindings where

import Foreign.C.Types
import Foreign.Ptr

data CLoopnaut

data CBindings = CBindings {
  create_loopnaut :: IO (Ptr CLoopnaut),
  set_buffer :: Ptr CLoopnaut -> Ptr CFloat -> Int -> IO ()
}
