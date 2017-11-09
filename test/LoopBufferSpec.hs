
module LoopBufferSpec where

import Test.Hspec
import Foreign.Ptr
import Foreign.C.Types
import System.Process
import CBindings

spec :: Spec
spec = do
  describe "loop_buffer" $ do
    it "c test-suite" $ do
      callCommand "gcc test/loop_buffer_spec.c -ljack -o test/a.out"
      callCommand "test/a.out"
      callCommand "rm test/a.out"
