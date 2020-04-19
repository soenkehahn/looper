{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RenderSpec where

import Development.Shake
import Looper
import Looper.Cli
import Sound.File.Sndfile as Snd
import Sound.File.Sndfile.Buffer.Vector as BV
import Test.Hspec
import Test.Mockery.Directory
import Test.Utils
import System.IO.Silently
import System.IO

renderToFile :: FilePath -> IO ()
renderToFile outputFile = do
  renderSampleToFile [0.1, 0.2, 0.3] outputFile

renderSampleToFile :: [Double] -> FilePath -> IO ()
renderSampleToFile samples outputFile = do
  Prelude.writeFile "foo.sh" $
    "#!/usr/bin/env bash\n" ++
    concat (map (\ sample -> "echo " ++ show sample ++ "\n") samples)
  unit $ cmd "chmod +x foo.sh"
  _ <- withMockBindings $ \ bindings -> do
    (mockFileWatcher, _mockFileSystem) <- mkMockFileWatcher
    run bindings mockFileWatcher (Render "foo.sh" outputFile)
  return ()

spec :: Spec
spec = describe "RenderSpec" $ around_ inTempDirectory $ do
  describe "when passing in an executable" $ do
    it "allows to render into an ogg file" $ do
      renderToFile "rendered.ogg"
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
      renderToFile "rendered.wav"
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
      renderToFile "rendered.unknown" `shouldThrow`
        errorCall "unknown audio file format: .unknown\nplease use .wav or .ogg"

    it "warns about invalid samples" $ do
      output <- hCapture_ [stderr] $ renderSampleToFile [-23, 42] "rendered.wav"
      let expected = unlines $
            "warning: some audio samples are outside the valid range:" :
            "min: -23.0, max: 42.0" :
            []
      output `shouldBe` expected
