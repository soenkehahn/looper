{-# OPTIONS_GHC -Wno-missing-fields #-}

module LoopnautSpec where

import Test.Hspec
import Data.IORef
import Foreign.Marshal.Array
import CBindings
import Loopnaut

spec :: Spec
spec = do
  describe "setBuffer" $ do
    it "plays back a buffer" $ do
      mockBuffer <- newIORef (error "uninitialized mockBuffer")
      let mockBindings = CBindings {
        create_loopnaut = do
          return (error "mock Ptr Buffer"),
        set_buffer = \ _ buffer len -> do
          writeIORef mockBuffer (buffer, len)
      }
      buffer <- create mockBindings
      setBuffer mockBindings buffer [1, 2, 3]
      (array, len) <- readIORef mockBuffer
      len `shouldBe` 3
      peekArray 3 array `shouldReturn` [1, 2, 3]

    it "allows to set a new buffer" $ do
      mockBuffer <- newIORef (error "uninitialized mockBuffer")
      let mockBindings = CBindings {
        create_loopnaut = do
          return (error "mock Ptr Buffer"),
        set_buffer = \ _ buffer len -> do
          writeIORef mockBuffer (buffer, len)
      }
      buffer <- create mockBindings
      setBuffer mockBindings buffer [1, 2, 3]
      setBuffer mockBindings buffer [4, 5, 6, 7]
      (array, len) <- readIORef mockBuffer
      len `shouldBe` 4
      peekArray 4 array `shouldReturn` [4, 5, 6, 7]
