
import Looper
import Looper.CBindings.FFI
import Looper.Cli
import Looper.FileWatcher

main :: IO ()
main = withCliArgs $ \ cliArgs -> do
  _ <- run cBindings fileWatcher cliArgs
  return ()
