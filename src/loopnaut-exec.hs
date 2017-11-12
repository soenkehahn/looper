
import Loopnaut
import CBindings.FFI
import WithCli

main :: IO ()
main = withCli $ \ cliArgs -> do
  _ <- run cBindings cliArgs
  return ()
