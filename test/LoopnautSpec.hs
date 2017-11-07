
module LoopnautSpec where

import Test.Hspec
import Data.IORef
import Foreign.Marshal.Array
import CBindings
import Loopnaut

spec =
  describe "loopBuffer" $ do
    it "plays back a buffer" $ do
      mockBuffer <- newIORef (error "uninitialized mockBuffer")
      let mockBindings = CBindings {
        loop_buffer = \ buffer length ->
          writeIORef mockBuffer (buffer, length)
      }
      loopBuffer mockBindings [1, 2, 3]
      (array, len) <- readIORef mockBuffer
      len `shouldBe` 3
      peekArray 3 array `shouldReturn` [1, 2, 3]
