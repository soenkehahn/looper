{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExecutableSpec where

import Control.Exception
import Control.Monad
import Data.List (isInfixOf)
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Development.Shake
import Foreign.C.Types
import System.IO
import System.IO.Silently
import Test.Hspec
import Test.Mockery.Directory
import Test.Utils

testRunWithFile :: Bool -> String -> IO () -> IO [([CFloat], Int)]
testRunWithFile executable fileContents test = do
  withMockBindings $ \ bindings -> timebox $ do
    writeFile "foo.sh" $ unindent fileContents
    when executable $ do
      unit $ cmd "chmod +x foo.sh"
    testRun bindings "foo.sh" [] $ \ _ -> test

spec :: Spec
spec = describe "ExecutableSpec" $ around_ inTempDirectory $ around_ (hSilence [stderr]) $ do
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
        output <- hCapture_ [stderr] $ testRunWithFile True [i|
          #!/usr/bin/env bash
          echo foo
        |] (return ())
        output `shouldContain` "foo.sh wrote a line to stdout that cannot be parsed as a number:\nfoo"

  describe "when file is neither executable nor a soundfile" $ do
    it "outputs a good error message" $ do
      output <- hCapture_ [stderr] $ testRunWithFile False [i|
        foo
      |] (return ())
      let expected libsndfileMessage = unindent $ [i|
            foo.sh is neither an executable (the executable flag is not set)
            nor is it a sound file:
              #{libsndfileMessage}|]
          isExpected output =
            expected "Format not recognised." `isInfixOf` output ||
            expected "File contains data in an unknown format." `isInfixOf` output
      output `shouldSatisfy`  isExpected

  describe "when the file does not exist" $ do
    it "outputs a good error message" $ do
      output <- hCapture_ [stderr] $ withMockBindings $ \ bindings -> timebox $ do
        testRun bindings "foo.sh" [] $ \ _ -> return ()
      output `shouldContain` "file not found: foo.sh"

  describe "terminal output" $ do
    it "outputs a message when starting and finishing to read the audio snippet" $ do
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

    it "warns about samples outside of the valid range" $ do
      output <- hCapture_ [stderr] $
        handle (\ (_ :: ErrorCall) -> return ()) $ do
          _ <- testRunWithFile True [i|
            #!/usr/bin/env bash
            echo 42
            echo -23
          |] (return ())
          return ()
      output `shouldBe` "reading audio snippet from foo.sh...\ndone\n" ++
        "warning: some audio samples are outside the valid range:\nmin: -23.0, max: 42.0\n"

  describe "executable failures with non-zero exit codes" $ do
    describe "when the initial executable fails" $ do
      it "plays nothing but waits for the executable to change" $ do
        result <- withMockBindings $ \ bindings -> do
          writeFile "foo.sh" $ unindent [i|
            #!/usr/bin/env bash
            exit 1
          |]
          unit $ cmd "chmod +x foo.sh"
          testRun bindings "foo.sh" [] $ \ mockFileSystem -> do
            write mockFileSystem "foo.sh" $ unindent [i|
              #!/usr/bin/env bash
              echo 42
            |]
        result `shouldBe` [([42], 1)]

    describe "when the executable is changed to a failing one" $ do
      it "keeps playing the previous loop" $ do
        result <- withMockBindings $ \ bindings -> do
          writeFile "foo.sh" $ unindent [i|
            #!/usr/bin/env bash
            echo 42
          |]
          unit $ cmd "chmod +x foo.sh"
          testRun bindings "foo.sh" [] $ \ mockFileSystem -> do
            write mockFileSystem "foo.sh" $ unindent [i|
              #!/usr/bin/env bash
              exit 1
            |]
        result `shouldBe` [([42], 1)]

      it "waits for another modification" $ do
        result <- withMockBindings $ \ bindings -> do
          writeFile "foo.sh" $ unindent [i|
            #!/usr/bin/env bash
            echo 42
          |]
          unit $ cmd "chmod +x foo.sh"
          testRun bindings "foo.sh" [] $ \ mockFileSystem -> do
            write mockFileSystem "foo.sh" $ unindent [i|
              #!/usr/bin/env bash
              exit 1
            |]
            write mockFileSystem "foo.sh" $ unindent [i|
              #!/usr/bin/env bash
              echo 23
            |]
        result `shouldBe` [([42], 1), ([23], 1)]

      it "outputs the thrown exception" $ do
        output <- hCapture_ [stderr] $ withMockBindings $ \ bindings -> do
          writeFile "foo.sh" $ unindent [i|
            #!/usr/bin/env bash
            echo 42
          |]
          unit $ cmd "chmod +x foo.sh"
          testRun bindings "foo.sh" [] $ \ mockFileSystem -> do
            write mockFileSystem "foo.sh" $ unindent [i|
              #!/usr/bin/env bash
              exit 1
            |]
        output `shouldContain` "failed with exit code 1"

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
