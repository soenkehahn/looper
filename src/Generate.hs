import Loopnaut.Utils

main :: IO ()
main = generateLoop "loop.wav" loop

loop :: Generator
loop phase = sin (phase * 180) * 0.1
