
import Loopnaut
import Loopnaut.CBindings.FFI
import WithCli

main :: IO ()
main = withCli $ \ cliArgs -> do
  _ <- run cBindings cliArgs
  return ()
