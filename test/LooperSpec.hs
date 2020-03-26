{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE ViewPatterns #-}

module LooperSpec where

import Development.Shake
import Foreign.C.Types
import Data.IORef
import Looper
import Looper.CBindings
import System.Directory
import System.FilePath
import System.IO
import System.IO.Silently
import Test.Hspec
import Test.Mockery.Directory
import Test.Utils
import qualified Data.Vector.Storable as Vec

spec :: Spec
spec = describe "LooperSpec" $ around_ (hSilence [stderr]) $ do
  describe "setBuffer" $ do
    it "plays back a buffer" $ do
      [(array, len)] <- withMockBindings $ \ bindings -> do
        buffer <- create_looper bindings
        setBuffer bindings buffer (Vec.fromList [1, 2, 3])
      len `shouldBe` 3
      array `shouldBe` [1, 2, 3]

    it "allows to set a new buffer" $ do
      (last -> (array, len)) <- withMockBindings $ \ bindings -> do
        buffer <- create_looper bindings
        setBuffer bindings buffer (Vec.fromList [1, 2, 3])
        setBuffer bindings buffer (Vec.fromList [4, 5, 6, 7])
      len `shouldBe` 4
      array `shouldBe` [4, 5, 6, 7]

  describe "run" $ do
    it "plays back a given file" $ do
      [(array, len)] <- testWhileLooperIsRunning $ \ _ -> do
        return ()
      len `shouldBe` 221
      take 3 array `shouldBe` [1.9686777e-2,5.7297602e-2,9.422311e-2]

    it "does execute the given action" $ do
      _ <- withMockBindings $ \ bindings -> do
        ref <- newIORef ""
        testRun bindings "test/test-sound-1.wav" [] $ \ _ ->
          writeIORef ref "foo"
        readIORef ref `shouldReturn` "foo"
      return ()

    it "does detect file changes and plays back the changed file" $ do
      buffers <- testWhileLooperIsRunning $ \ mockFileSystem -> do
        cp mockFileSystem "test-sound-2.wav" "current.wav"
      length buffers `shouldBe` 2
      let [_, last] = buffers
      snd last `shouldBe` 221
      take 3 (fst last) `shouldBe` [5.5871088e-2,0.16261064,0.26740527]

    it "does not replace the buffer when the file is deleted" $ do
      buffers <- testWhileLooperIsRunning $ \ mockFileSystem -> do
        rm mockFileSystem "current.wav"
      length buffers `shouldBe` 1

    it "does replace the buffer after deletion with a newly created file" $ do
      buffers <- testWhileLooperIsRunning $ \ mockFileSystem -> do
        rm mockFileSystem "current.wav"
        cp mockFileSystem "test-sound-2.wav" "current.wav"
      length buffers `shouldBe` 2
      snd (last buffers) `shouldBe` 221
      take 3 (fst (last buffers)) `shouldBe` [5.5871088e-2,0.16261064,0.26740527]

testWhileLooperIsRunning :: (MockFileSystem -> IO ()) -> IO [([CFloat], Int)]
testWhileLooperIsRunning test = do
  repoDir <- getCurrentDirectory
  inTempDirectory $ do
    withMockBindings $ \ bindings ->
      timebox $ do
        unit $ cmd "cp" (repoDir </> "test/test-sound-1.wav") "test-sound-1.wav"
        unit $ cmd "cp" (repoDir </> "test/test-sound-2.wav") "test-sound-2.wav"
        unit $ cmd "cp test-sound-1.wav current.wav"
        testRun bindings "current.wav" [] test
