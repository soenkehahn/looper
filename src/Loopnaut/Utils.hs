module Loopnaut.Utils
  ( Generator
  , generateLoop
  ) where

import Data.Monoid
import Data.Ratio
import Data.Vector.Storable as V
import Prelude hiding (writeFile)
import Sound.File.Sndfile (getFileInfo, samplerate, writeFile)
import Sound.File.Sndfile.Buffer.Vector as V

-- * state
data State = State
  { loopTime :: Rational
  , phase :: Double
  }

initial :: State
initial = State 0 0

type Generator = Double -> Double

generateLoop :: FilePath -> Generator -> IO ()
generateLoop file generator = do
  putStr ("generating " <> file <> "...")
  info <- getFileInfo file
  _ <- writeFile info file $ mkBuffer generator (fromIntegral (samplerate info))
  putStrLn "done"

mkBuffer :: Generator -> Integer -> V.Buffer Double
mkBuffer generator samplerate =
  toBuffer $ (unfoldr (runGenerator generator samplerate) initial)

runGenerator :: Generator -> Integer -> State -> Maybe (Double, State)
runGenerator generator samplerate (State loopTime phase) =
  if loopTime >= 1
    then Nothing
    else Just $
         let newPhase = foldRadians (phase + tau * 1 / fromIntegral samplerate)
         in (generator phase, State (loopTime + 1 % (5 * samplerate)) newPhase)

-- * math
tau :: Double
tau = 2 * pi

foldRadians :: Double -> Double
foldRadians x =
  if x >= tau
    then foldRadians (x - tau)
    else x
