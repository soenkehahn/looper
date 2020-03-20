module Looper.CBindings where

import Foreign.C.Types
import Foreign.Ptr

data CLooper

data CBindings = CBindings {
  create_looper :: IO (Ptr CLooper),
  set_buffer :: Ptr CLooper -> Ptr CFloat -> Int -> IO ()
}
