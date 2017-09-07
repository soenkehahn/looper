{-# OPTIONS_GHC -Wall -Werror #-}

import Loopnaut.Utils

main :: IO ()
main = generateLoop "loop.wav" 5 loop

loop :: Generator
loop phase = mix (map (\freq -> sin (phase * freq) * 0.2) [200, 300, 400, 500])

mix :: [Double] -> Double
mix list =
  case list of
    [] -> 0
    _ -> sum list / fromIntegral (length list)
