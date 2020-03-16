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
import Test.Utils

renderFile :: FilePath -> IO ()
renderFile outputFile = do
  Prelude.writeFile "foo.sh" $ unindent [i|
    #!/usr/bin/env bash
    echo 1
    echo 2
    echo 3
  |]
  unit $ cmd "chmod +x foo.sh"
  _ <- withMockBindings $ \ bindings -> do
    (mockFileWatcher, _mockFileSystem) <- mkMockFileWatcher
    run bindings mockFileWatcher (Render "foo.sh" outputFile)
  return ()

spec :: Spec
spec = describe "RenderSpec" $ around_ inTempDirectory $ do
  describe "when passing in an executable" $ do
    it "allows to render into an ogg file" $ do
      renderFile "rendered.ogg"
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

    it "allows to render into a wav file" $ do
      renderFile "rendered.wav"
      (info, _ :: Maybe (BV.Buffer Double)) <- Snd.readFile "rendered.wav"
      let expected = Info {
            frames = 3,
            samplerate = 44100,
            channels = 1,
            format = Format {
              headerFormat = HeaderFormatWav,
              sampleFormat = SampleFormatPcm16,
              endianFormat = EndianFile
            },
            sections = 1,
            seekable = True
          }
      info `shouldBe` expected

    it "throws a good error when trying to render into a file with an unknown extension" $ do
      renderFile "rendered.unknown" `shouldThrow`
        errorCall "unknown audio file format: .unknown\nplease use .wav or .ogg"
