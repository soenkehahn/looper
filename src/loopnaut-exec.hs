
import Loopnaut
import Loopnaut.CBindings.FFI
import Loopnaut.Cli
import Loopnaut.FileWatcher.Implementation

main :: IO ()
main = withCliArgs $ \ cliArgs -> do
  _ <- run cBindings fileWatcher cliArgs
  return ()
