{-# OPTIONS_GHC -Wno-missing-fields #-}

module LoopnautSpec where

import Test.Hspec
import Data.IORef
import Foreign.Marshal.Array
import CBindings
import Loopnaut
import Foreign.C.Types

spec :: Spec
spec = do
  describe "setBuffer" $ do
    it "plays back a buffer" $ do
      (array, len) <- withMockBindings $ \ bindings -> do
        buffer <- create bindings
        setBuffer bindings buffer [1, 2, 3]
      len `shouldBe` 3
      array `shouldBe` [1, 2, 3]

    it "allows to set a new buffer" $ do
      (array, len) <- withMockBindings $ \ bindings -> do
        buffer <- create bindings
        setBuffer bindings buffer [1, 2, 3]
        setBuffer bindings buffer [4, 5, 6, 7]
      len `shouldBe` 4
      array `shouldBe` [4, 5, 6, 7]

  describe "run" $ do
    it "plays back a given file" $ do
      (array, len) <- withMockBindings $ \ bindings -> do
        run bindings (CliArgs "test/test-sound.wav")
      len `shouldBe` 1487
      take 3 array `shouldBe` [1.9686777e-2,5.7297602e-2,9.422311e-2]

withMockBindings :: (CBindings -> IO a) -> IO ([CFloat], Int)
withMockBindings action = do
  mockBuffer <- newIORef (error "uninitialized mockBuffer")
  let mockBindings = CBindings {
    create_loopnaut = do
      return (error "mock Ptr Buffer"),
    set_buffer = \ _ buffer len -> do
      writeIORef mockBuffer (buffer, len)
  }
  action mockBindings
  (array, len) <- readIORef mockBuffer
  peeked <- peekArray len array
  return (peeked, len)
