module Looper.CliSpec where

import Control.Exception
import Looper.Cli
import System.Environment
import Test.Hspec

testWithArgs :: [String] -> (CliArgs -> IO ()) -> IO ()
testWithArgs args test = do
  withArgs args (withCliArgs test)

spec :: Spec
spec = describe "CliSpec" $ do
  describe "withCliArgs" $ do
    it "parses the main file as a positional argument" $ do
      testWithArgs ["foo"] $ \ args -> do
        args `shouldBe` Loop "foo" [] DontNormalize

    it "complains about no positional argument" $ do
      let command = testWithArgs [] $ \ _ -> do
            throwIO $ ErrorCall "shouldn't happen"
      command `shouldThrow` errorCall "no main file given"

    it "complains about more than 1 positional argument" $ do
      let command = testWithArgs ["foo", "bar"] $ \ _ -> do
            throwIO $ ErrorCall "shouldn't happen"
      command `shouldThrow`
        errorCall "multiple main files given: foo, bar\nplease provide only one main file"

    it "allows to pass in a watched file with --watch" $ do
      testWithArgs ["foo", "--watch", "bar"] $ \ args -> do
        cliArgsWatched args `shouldBe` ["bar"]

    it "allows to pass in multiple watched files with --watch" $ do
      testWithArgs ["foo", "--watch", "bar", "--watch", "baz"] $ \ args -> do
        cliArgsWatched args `shouldBe` ["bar", "baz"]

    it "allows to pass in an output file with --render" $ do
      testWithArgs ["foo", "--render", "bar"] $ \ args -> do
        args `shouldBe` Render "foo" "bar" DontNormalize

    describe "normalization" $ do
      it "doesn't normalize by default" $ do
        testWithArgs ["foo"] $ \ args -> do
          cliArgsNormalization args `shouldBe` DontNormalize

      it "allows to switch on normalization with --normalize" $ do
        testWithArgs ["foo", "--normalize"] $ \ args -> do
          cliArgsNormalization args `shouldBe` Normalize

      it "accepts -n as a shortcut" $ do
        testWithArgs ["foo", "-n"] $ \ args -> do
          cliArgsNormalization args `shouldBe` Normalize
