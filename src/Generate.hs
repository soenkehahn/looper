import Data.Ratio
import Data.Vector.Storable as V
import Prelude hiding (writeFile)
import Sound.File.Sndfile (getFileInfo, samplerate, writeFile)
import Sound.File.Sndfile.Buffer.Vector as V
import System.Random

main :: IO ()
main = do
  putStr "generating..."
  overwrite "loop.wav"
  putStrLn "done"

overwrite :: FilePath -> IO ()
overwrite file = do
  info <- getFileInfo file
  _ <- writeFile info file =<< mkBuffer (fromIntegral (samplerate info))
  return ()

mkBuffer :: Integer -> IO (V.Buffer Double)
mkBuffer samplerate = return $ toBuffer $ vector samplerate

vector samplerate = (unfoldr (gen samplerate) initial)

data State = State
  { loopTime :: Rational
  , phase :: Double
  }

initial :: State
initial = State 0 0

gen :: Integer -> State -> Maybe (Double, State)
gen samplerate (State loopTime phase) =
  if loopTime >= 1
    then Nothing
    else Just $
         let newPhase = foldRadians (phase + tau * 1 / fromIntegral samplerate)
         in ( sin (phase * 180) * 0.1
            , State (loopTime + 1 % (5 * samplerate)) newPhase)

foldRadians :: Double -> Double
foldRadians x =
  if x >= tau
    then foldRadians (x - tau)
    else x

tau = 2 * pi
