{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Loopnaut.UtilsSpec where

import Data.Vector.Storable as V
import Loopnaut.Utils
import Sound.File.Sndfile as SF (readFile)
import Sound.File.Sndfile.Buffer.Vector
import System.Directory
import System.FilePath
import Test.Hspec
import Test.Mockery.Directory

spec :: Spec
spec = do
  describe "writeSndFile" $ do
    it "writes a soundfile to disk" $ do
      repoDir <- getCurrentDirectory
      let testFile = repoDir </> "test/test-sound-1.wav"
      inTempDirectory $ do
        copyFile testFile "foo.wav"
        writeSndFile "foo.wav" [0.25, 0.25]
        (_, Just buffer) <- SF.readFile "foo.wav"
        V.forM_ (fromBuffer buffer) $ \(n :: Double) -> n `shouldBe` 0.25
