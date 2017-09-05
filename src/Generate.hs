
import Data.Vector.Storable as V
import Prelude hiding (writeFile)
import Sound.File.Sndfile (getFileInfo, writeFile)
import Sound.File.Sndfile.Buffer.Vector as V
import System.Random

main :: IO ()
main = do
  overwrite "loop.wav"
  putStrLn "done"

overwrite :: FilePath -> IO ()
overwrite file = do
  info <- getFileInfo file
  _ <- writeFile info file =<< mkBuffer
  return ()

mkBuffer :: IO (V.Buffer Double)
mkBuffer = return $ toBuffer vector

vector = (unfoldr gen initial)

data State = State {
  sampleCount :: Int,
  phase :: Double
}

initial :: State
initial = State 0 0

samplerate :: Num n => n
samplerate = 44100

gen :: State -> Maybe (Double, State)
gen (State sampleCount phase) =
  if sampleCount >= samplerate * 5
    then Nothing
    else Just $
      let newPhase = foldRadians (phase + tau * 1 / samplerate)
      in (sin (phase * 150) * 0.3, State (sampleCount + 1) newPhase)

foldRadians :: Double -> Double
foldRadians x =
  if x >= tau
    then foldRadians (x - tau)
    else x

tau = 2 * pi
