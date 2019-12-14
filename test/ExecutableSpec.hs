{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExecutableSpec where

import Test.Hspec
import Control.Monad
import Development.Shake
import Test.Mockery.Directory
import Loopnaut
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Foreign.C.Types
import Utils

runWithFile :: Bool -> String -> IO [([CFloat], Int)]
runWithFile executable fileContents = do
  withMockBindings $ \ bindings -> timebox $ do
    writeFile "foo.sh" $ unindent fileContents
    when executable $ do
      unit $ cmd "chmod +x foo.sh"
    run bindings (CliArgs (File "foo.sh"))

spec :: Spec
spec = around_ inTempDirectory $ do
  describe "when given an executable file" $ do
    it "executes the file and reads the sound samples from its stdout" $ do
      result <- runWithFile True [i|
        #!/usr/bin/env bash
        echo 1
        echo 2
        echo 3
      |]
      result `shouldBe` [([1, 2, 3], 3)]

    describe "when the output format is invalid" $ do
      it "outputs a good error message" $ do
        let command = runWithFile True [i|
                #!/usr/bin/env bash
                echo foo
              |]
        command `shouldThrow` (errorCall "foo.sh wrote a line to stdout that cannot be parsed as a number:\nfoo")

    describe "when the file returns a non-zero exit code" $ do
      it "outputs a good error message" $ do
        let command = runWithFile True [i|
                #!/usr/bin/env bash
                false
              |]
        command `shouldThrow` (errorCall "foo.sh failed with exit code 1")

  describe "when file is neither executable nor a soundfile" $ do
    it "outputs a good error message" $ do
      let command = runWithFile False [i|
              foo
            |]
      let expected = unindent $ [i|
        foo.sh is neither an executable (the executable flag is not set)
        nor is it a sound file:
          File contains data in an unknown format.|]
      command `shouldThrow` errorCall expected

  describe "when the file does not exist" $ do
    it "outputs a good error message" $ do
      let command = withMockBindings $ \ bindings -> timebox $ do
            run bindings (CliArgs (File "foo.sh"))
      command `shouldThrow` errorCall "file not found: foo.sh"
