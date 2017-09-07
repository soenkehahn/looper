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

generateLoop :: FilePath -> Rational -> Generator -> IO ()
generateLoop file seconds generator = do
  putStr ("generating " <> file <> "...")
  info <- getFileInfo file
  _ <-
    writeFile info file $
    mkBuffer seconds (fromIntegral (samplerate info)) generator
  putStrLn "done"

mkBuffer :: Rational -> Integer -> Generator -> V.Buffer Double
mkBuffer seconds samplerate generator =
  toBuffer $ (unfoldr (runGenerator seconds samplerate generator) initial)

runGenerator ::
     Rational -> Integer -> Generator -> State -> Maybe (Double, State)
runGenerator seconds samplerate generator (State loopTime phase) =
  if loopTime >= 1
    then Nothing
    else Just $
         let newPhase = foldRadians (phase + tau * 1 / fromIntegral samplerate)
         in ( generator phase
            , State
                (loopTime + 1 / (seconds * fromIntegral samplerate))
                newPhase)

-- * math
tau :: Double
tau = 2 * pi

foldRadians :: Double -> Double
foldRadians x =
  if x >= tau
    then foldRadians (x - tau)
    else x
