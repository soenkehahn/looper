module Looper.File.ExecutableSpec where

import Data.ByteString.Char8 as BS
import Data.String.Conversions
import Looper.File.Executable
import System.IO
import System.IO.Silently
import Test.Hspec

spec :: Spec
spec = do
  describe "parse" $ do
    it "reports progress to stderr" $ do
      let input = BS.unlines $ Prelude.replicate 4410 (cs "0")
      output <- hCapture_ [stderr] $ _parse "file" input
      output `shouldBe` "read 0.1 seconds\n"

    it "reports multiple times by overwriting the recent report line" $ do
      let input = BS.unlines $ Prelude.replicate (4410 * 2) (cs "0")
      output <- hCapture_ [stderr] $ _parse "file" input
      output `shouldBe` "read 0.1 seconds\rread 0.2 seconds\n"

    it "doesn't print anything when input is shorter than 0.1 seconds" $ do
      let input = BS.unlines $ Prelude.replicate (4410 - 1) (cs "0")
      output <- hCapture_ [stderr] $ _parse "file" input
      output `shouldBe` ""
