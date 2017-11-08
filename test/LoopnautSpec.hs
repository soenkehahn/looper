{-# OPTIONS_GHC -Wno-missing-fields #-}

module LoopnautSpec where

import Test.Hspec
import Data.IORef
import Foreign.Marshal.Array
import CBindings
import Loopnaut

spec :: Spec
spec = do
  describe "loopBuffer" $ do
    it "plays back a buffer" $ do
      mockBuffer <- newIORef (error "uninitialized mockBuffer")
      let mockBindings = CBindings {
        loop_buffer = \ buffer length -> do
          writeIORef mockBuffer (buffer, length)
          return (error "mock Ptr Buffer")
      }
      loopBuffer mockBindings [1, 2, 3]
      (array, len) <- readIORef mockBuffer
      len `shouldBe` 3
      peekArray 3 array `shouldReturn` [1, 2, 3]

  describe "setBuffer" $ do
    it "allows to set a new buffer" $ do
      mockBuffer <- newIORef (error "uninitialized mockBuffer")
      let mockBindings = CBindings {
        set_buffer = \ _ buffer length -> do
          writeIORef mockBuffer (buffer, length)
          return ()
      }
      setBuffer mockBindings (error "mock buffer") [1, 2, 3]
      (array, len) <- readIORef mockBuffer
      len `shouldBe` 3
      peekArray 3 array `shouldReturn` [1, 2, 3]
