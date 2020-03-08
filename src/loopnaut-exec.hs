
import Loopnaut
import Loopnaut.CBindings.FFI
import Loopnaut.Cli
import Loopnaut.FileWatcher

main :: IO ()
main = withCliArgs $ \ cliArgs -> do
  _ <- run cBindings fileWatcher cliArgs
  return ()
