{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExecutableSpec where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Development.Shake
import Foreign.C.Types
import Loopnaut
import System.IO
import System.IO.Silently
import Test.Hspec
import Test.Mockery.Directory
import Utils
import Cli

runWithFile :: Bool -> String -> IO [([CFloat], Int)]
runWithFile executable fileContents = do
  withMockBindings $ \ bindings -> timebox $ do
    writeFile "foo.sh" $ unindent fileContents
    when executable $ do
      unit $ cmd "chmod +x foo.sh"
    run bindings (CliArgs "foo.sh" [])

spec :: Spec
spec = around_ inTempDirectory $ around_ (hSilence [stderr]) $ do
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
            run bindings (CliArgs "foo.sh" [])
      command `shouldThrow` errorCall "file not found: foo.sh"

  describe "terminal output" $ do
    it "outputs a message when starting to read the audio snippet" $ do
      output <-
        hCapture_ [stderr] $
        handle (\ (_ :: ErrorCall) -> return ()) $ do
          _ <- runWithFile True [i|
            #!/usr/bin/env bash
            echo 1
            false
          |]
          return ()
      output `shouldBe` "reading audio snippet from foo.sh...\n"

    it "outputs a message when finishing to read the audio snippet" $ do
      output <-
        hCapture_ [stderr] $
        handle (\ (_ :: ErrorCall) -> return ()) $ do
          _ <- runWithFile True [i|
            #!/usr/bin/env bash
            echo 1
          |]
          return ()
      output `shouldBe` "reading audio snippet from foo.sh...\ndone\n"

    it "relays output to stderr from the executables" $ do
      output <-
        hCapture_ [stderr] $
        handle (\ (_ :: ErrorCall) -> return ()) $ do
          _ <- runWithFile True [i|
            #!/usr/bin/env bash
            echo foo 1>&2
          |]
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
        _ <- forkIO $ do
          threadDelay 4000
          writeFile "bar" "foo"
        run bindings (CliArgs "foo.sh" [])
      output `shouldBe` "reading audio snippet from foo.sh...\ndone\n"

    it "allows to watch additional files" $ do
      output <- hCapture_ [stderr] $ withMockBindings $ \ bindings -> timebox $ do
        writeFile "foo.sh" $ unindent [i|
          #!/usr/bin/env bash
          echo 1
        |]
        unit $ cmd "chmod +x foo.sh"
        _ <- forkIO $ do
          threadDelay 8000
          writeFile "bar" "foo"
        run bindings (CliArgs "foo.sh" ["bar"])
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
        _ <- forkIO $ do
          threadDelay 8000
          writeFile "bar/baz" "foo"
        run bindings (CliArgs "foo.sh" ["bar/baz"])
      output `shouldBe`
        "reading audio snippet from foo.sh...\ndone\n" ++
        "bar/baz changed, reading audio snippet from foo.sh...\ndone\n"
