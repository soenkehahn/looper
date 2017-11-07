{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Concurrent

main :: IO ()
main = do
  c_start_sine
  forever $ threadDelay 1000000

foreign import ccall "start_sine" c_start_sine :: IO ()
