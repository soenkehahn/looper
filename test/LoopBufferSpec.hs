{-# LANGUAGE ViewPatterns #-}

module LoopBufferSpec where

import Control.Exception
import Development.Shake
import Test.Hspec

spec :: Spec
spec = do
  describe "loop_buffer" $ do
    it "c test-suite" $ do
      Stdout (words -> libsFlags) <- cmd "pkg-config --libs portaudio-2.0"
      bracket
        (unit $ cmd "gcc test/loop_buffer_spec.c" libsFlags "-o test/a.out")
        (\ () -> unit $ cmd "rm test/a.out")
        (\ () -> unit $ cmd "test/a.out")
