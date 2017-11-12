
import Loopnaut
import CBindings.FFI
import WithCli
import Control.Monad
import Control.Concurrent

main :: IO ()
main = do
  withCli (run cBindings)
  forever $ threadDelay 1000000
