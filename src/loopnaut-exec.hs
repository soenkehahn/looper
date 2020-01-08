
import Loopnaut
import Loopnaut.CBindings.FFI
import Cli

main :: IO ()
main = withCliArgs $ \ cliArgs -> do
  _ <- run cBindings cliArgs
  return ()
