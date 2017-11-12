{-# OPTIONS_GHC -Wall  -Werror #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE LambdaCase #-}

import Loopnaut.Utils
import System.Random hiding (random)
import Data.Function

main :: IO ()
main = do
  writeSndFile "loop.wav" loop

loop :: Generator
loop = d-z-z-d-s-z-(d+c)-s-d-z-d-p-(d+c)-z-(d+c)-p
  where
    a - b = a ++ b
    a + b = zipWith (Prelude.+) a b
    d = n drum
    z = n []
    s = n $ snare 2
    c = n $ map (* 0.2) $ snare 1
    p = n $ map (* 0.1) $ snare 0
    n = pad (length drum) . take (length drum)

drum =
  fadeOut 100 $
  take (round (44100 * 0.17)) $
  fadeIn 80 $
  take 44100 $
  zipWith (\ phase time -> sin ((max 0 (80 - 500 * time ** 2)) * phase) * 1)
  phase time

pad n signal = signal ++ replicate (max 0 (n - length signal)) 0

random = unf (mkStdGen 42) (randomR (-1, 1))

unf :: s -> (s -> (a, s)) -> [a]
unf s f =
  let (a, s') = f s
  in a : unf s' f

snare n =
  random
   & take 300
   & fadeOut 100
   & fadeIn 100
   & apply n octaver

apply :: Int -> (a -> a) -> a -> a
apply n f a = case n of
  0 -> a
  n -> apply (n - 1) f (f a)

octaver = \ case
  (a : b : r) -> a : ((a + b) / 2) : octaver (b : r)
  x -> x

phase :: [Double]
phase = mconcat $ repeat _phase

_phase :: [Double]
_phase = [0, tau / 44100 ..]

time :: [Double]
time = [0, 1 / 44100 ..]

tau = 2 * pi

fadeIn n signal =
  zipWith (*) (take n signal) [0, 1 / fromIntegral n ..] ++
  drop n signal

fadeOut n =
  reverse . fadeIn n . reverse
