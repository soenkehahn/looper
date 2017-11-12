module Loopnaut.Utils
  ( Generator
  , writeSndFile
  ) where

import Data.Monoid
import Data.Vector.Storable as V
import Prelude hiding (writeFile)
import Sound.File.Sndfile (getFileInfo, writeFile)
import Sound.File.Sndfile.Buffer.Vector as V

type Generator = [Double]

writeSndFile :: FilePath -> Generator -> IO ()
writeSndFile file generator = do
  putStr ("generating " <> file <> "...")
  info <- getFileInfo file
  _ <- writeFile info file $ V.toBuffer $ V.fromList generator
  putStrLn "done"
