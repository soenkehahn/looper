{-# LANGUAGE ScopedTypeVariables #-}

module Loopnaut.FromSndFile where

import Control.Exception
import Data.Vector.Storable (toList)
import qualified Sound.File.Sndfile as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as BV

data FromSndfile
  = SndFileSuccess [Double]
  | SndFileError String

readFromSndfile :: FilePath -> IO FromSndfile
readFromSndfile file = do
  result <- try (Snd.readFile file)
  return $ case result of
    Left (e :: Snd.Exception) -> SndFileError $ Snd.errorString e
    Right (_info, Just buffer) -> SndFileSuccess (toList (BV.fromBuffer buffer))
    Right (_info, Nothing) -> SndFileError "empty file"
