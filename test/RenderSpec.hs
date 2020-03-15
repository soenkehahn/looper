{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RenderSpec where

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Development.Shake
import Loopnaut
import Loopnaut.Cli
import Sound.File.Sndfile as Snd
import Sound.File.Sndfile.Buffer.Vector as BV
import Test.Hspec
import Test.Mockery.Directory
import Utils

spec :: Spec
spec = describe "RenderSpec" $ do
  it "allows to render into an ogg file" $ inTempDirectory $ do
    _ <- withMockBindings $ \ bindings -> do
      Prelude.writeFile "foo.sh" $ unindent [i|
        #!/usr/bin/env bash
        echo 1
        echo 2
        echo 3
      |]
      unit $ cmd "chmod +x foo.sh"
      (mockFileWatcher, _mockFileSystem) <- mkMockFileWatcher
      run bindings mockFileWatcher (Render "foo.sh" "rendered.ogg")
    (info, _ :: Maybe (BV.Buffer Double)) <- Snd.readFile "rendered.ogg"
    let expected = Info {
          frames = 3,
          samplerate = 44100,
          channels = 1,
          format = Format {
            headerFormat = HeaderFormatOgg,
            sampleFormat = SampleFormatVorbis,
            endianFormat = EndianFile
          },
          sections = 1,
          seekable = True
        }
    info `shouldBe` expected
