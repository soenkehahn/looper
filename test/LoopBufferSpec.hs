module LoopBufferSpec where

import Control.Exception
import System.Process
import Test.Hspec

spec :: Spec
spec = do
  describe "loop_buffer" $ do
    it "c test-suite" $ do
      bracket
        (callCommand "gcc test/loop_buffer_spec.c -ljack -o test/a.out")
        (\() -> callCommand "rm test/a.out")
        (\() -> callCommand "test/a.out")
