{-# LANGUAGE ScopedTypeVariables #-}

module Loopnaut.Snippet.FromSndFile where

import Control.Exception
import Data.Vector.Storable as V
import Sound.File.Sndfile as Snd
import Sound.File.Sndfile.Buffer.Vector

data FromSndfile
  = SndFileSuccess [Double]
  | SndFileError String

readFromSndfile :: FilePath -> IO FromSndfile
readFromSndfile file = do
  result <- try (Snd.readFile file)
  return $ case result of
    Left (e :: Snd.Exception) -> SndFileError $ errorString e
    Right (_info, Just buffer) -> SndFileSuccess (toList (fromBuffer buffer))
    Right (_info, Nothing) -> SndFileError "empty file"

writeToSndfile :: FilePath -> [Double] -> IO ()
writeToSndfile file buffer = do
  let vector = fromList buffer
      info = Info {
        frames = (V.length vector),
        samplerate = 44100,
        channels = 1,
        format = Format {
          headerFormat = HeaderFormatOgg,
          sampleFormat = SampleFormatVorbis,
          endianFormat = EndianFile
        },
        sections = 1,
        seekable = False
      }
  _ <- Snd.writeFile info file (toBuffer vector)
  return ()
