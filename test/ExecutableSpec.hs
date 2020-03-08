{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExecutableSpec where

import Control.Exception
import Control.Monad
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Development.Shake
import Foreign.C.Types
import System.IO
import System.IO.Silently
import Test.Hspec
import Test.Mockery.Directory
import Utils

testRunWithFile :: Bool -> String -> IO () -> IO [([CFloat], Int)]
testRunWithFile executable fileContents test = do
  withMockBindings $ \ bindings -> timebox $ do
    writeFile "foo.sh" $ unindent fileContents
    when executable $ do
      unit $ cmd "chmod +x foo.sh"
    testRun bindings "foo.sh" [] $ \ _ -> test

spec :: Spec
spec = around_ inTempDirectory $ around_ (hSilence [stderr]) $ do
  describe "when given an executable file" $ do
    it "executes the file and reads the sound samples from its stdout" $ do
      result <- testRunWithFile True [i|
        #!/usr/bin/env bash
        echo 1
        echo 2
        echo 3
      |] (return ())
      result `shouldBe` [([1, 2, 3], 3)]

    describe "when the output format is invalid" $ do
      it "outputs a good error message" $ do
        let command = testRunWithFile True [i|
                #!/usr/bin/env bash
                echo foo
              |] (return ())
        command `shouldThrow` (errorCall "foo.sh wrote a line to stdout that cannot be parsed as a number:\nfoo")

    describe "when the file returns a non-zero exit code" $ do
      it "outputs a good error message" $ do
        let command = testRunWithFile True [i|
                #!/usr/bin/env bash
                false
              |] (return ())
        command `shouldThrow` (errorCall "foo.sh failed with exit code 1")

  describe "when file is neither executable nor a soundfile" $ do
    it "outputs a good error message" $ do
      let command = testRunWithFile False [i|
              foo
            |] (return ())
      let expected = unindent $ [i|
        foo.sh is neither an executable (the executable flag is not set)
        nor is it a sound file:
          File contains data in an unknown format.|]
      command `shouldThrow` errorCall expected

  describe "when the file does not exist" $ do
    it "outputs a good error message" $ do
      let command = withMockBindings $ \ bindings -> timebox $ do
            testRun bindings "foo.sh" [] $ \ _ -> return ()
      command `shouldThrow` errorCall "file not found: foo.sh"

  describe "terminal output" $ do
    it "outputs a message when starting to read the audio snippet" $ do
      output <-
        hCapture_ [stderr] $
        handle (\ (_ :: ErrorCall) -> return ()) $ do
          _ <- testRunWithFile True [i|
            #!/usr/bin/env bash
            echo 1
            false
          |] (return ())
          return ()
      output `shouldBe` "reading audio snippet from foo.sh...\n"

    it "outputs a message when finishing to read the audio snippet" $ do
      output <-
        hCapture_ [stderr] $
        handle (\ (_ :: ErrorCall) -> return ()) $ do
          _ <- testRunWithFile True [i|
            #!/usr/bin/env bash
            echo 1
          |] (return ())
          return ()
      output `shouldBe` "reading audio snippet from foo.sh...\ndone\n"

    it "relays output to stderr from the executables" $ do
      output <-
        hCapture_ [stderr] $
        handle (\ (_ :: ErrorCall) -> return ()) $ do
          _ <- testRunWithFile True [i|
            #!/usr/bin/env bash
            echo foo 1>&2
          |] (return ())
          return ()
      output `shouldBe` "reading audio snippet from foo.sh...\nfoo\ndone\n"

  describe "file change detection" $ do
    it "only watches the given file" $ do
      output <- hCapture_ [stderr] $ withMockBindings $ \ bindings -> timebox $ do
        writeFile "foo.sh" $ unindent [i|
          #!/usr/bin/env bash
          echo 1
        |]
        unit $ cmd "chmod +x foo.sh"
        testRun bindings "foo.sh" [] $ \ mockFileSystem -> do
          write mockFileSystem "bar" "foo"
      output `shouldBe` "reading audio snippet from foo.sh...\ndone\n"

    it "allows to watch additional files" $ do
      output <- hCapture_ [stderr] $ withMockBindings $ \ bindings -> timebox $ do
        writeFile "foo.sh" $ unindent [i|
          #!/usr/bin/env bash
          echo 1
        |]
        unit $ cmd "chmod +x foo.sh"
        testRun bindings "foo.sh" ["bar"] $ \ mockFileSystem -> do
          write mockFileSystem "bar" "foo"
      output `shouldBe`
        "reading audio snippet from foo.sh...\ndone\n" ++
        "bar changed, reading audio snippet from foo.sh...\ndone\n"

    it "allows to watch additional files in subdirectories" $ do
      output <- hCapture_ [stderr] $ withMockBindings $ \ bindings -> timebox $ do
        writeFile "foo.sh" $ unindent [i|
          #!/usr/bin/env bash
          echo 1
        |]
        unit $ cmd "chmod +x foo.sh"
        unit $ cmd "mkdir bar"
        testRun bindings "foo.sh" ["bar/baz"] $ \ mockFileSystem -> do
          write mockFileSystem "bar/baz" "foo"
      output `shouldBe`
        "reading audio snippet from foo.sh...\ndone\n" ++
        "bar/baz changed, reading audio snippet from foo.sh...\ndone\n"
