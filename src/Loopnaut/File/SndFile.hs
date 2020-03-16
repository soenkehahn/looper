{-# LANGUAGE ScopedTypeVariables #-}

module Loopnaut.File.SndFile where

import Control.Exception
import Data.Vector.Storable as V
import Sound.File.Sndfile as Snd
import Sound.File.Sndfile.Buffer.Vector
import System.FilePath

data FromSndfile
  = SndFileSuccess [Double]
  | SndFileError String

readFromSndFile :: FilePath -> IO FromSndfile
readFromSndFile file = do
  result <- try (Snd.readFile file)
  return $ case result of
    Left (e :: Snd.Exception) -> SndFileError $ errorString e
    Right (_info, Just buffer) -> SndFileSuccess (toList (fromBuffer buffer))
    Right (_info, Nothing) -> SndFileError "empty file"

writeToSndFile :: FilePath -> [Double] -> IO ()
writeToSndFile file buffer = do
  format <- case takeExtension file of
    ".ogg" -> return $ Format {
      headerFormat = HeaderFormatOgg,
      sampleFormat = SampleFormatVorbis,
      endianFormat = EndianFile
    }
    ".wav" -> return $ Format {
      headerFormat = HeaderFormatWav,
      sampleFormat = SampleFormatPcm16,
      endianFormat = EndianFile
    }
    extension -> throwIO $ ErrorCall $
      "unknown audio file format: " <> extension <> "\n" <>
      "please use .wav or .ogg"
  let vector = fromList buffer
      info = Info {
        frames = (V.length vector),
        samplerate = 44100,
        channels = 1,
        format = format,
        sections = 1,
        seekable = False
      }
  _ <- Snd.writeFile info file (toBuffer vector)
  return ()
