module Loopnaut.CliSpec where

import Control.Exception
import Loopnaut.Cli
import System.Environment
import Test.Hspec

testWithArgs :: [String] -> (CliArgs -> IO ()) -> IO ()
testWithArgs args test = do
  withArgs args (withCliArgs test)

spec :: Spec
spec = do
  describe "withCliArgs" $ do
    it "parses the main file as a positional argument" $ do
      testWithArgs ["foo"] $ \ args -> do
        file <- mkFile "foo"
        args `shouldBe` CliArgs file []

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
        bar <- mkFile "bar"
        cliArgsWatched args `shouldBe` [bar]

    it "allows to pass in multiple watched files with --watch" $ do
      testWithArgs ["foo", "--watch", "bar", "--watch", "baz"] $ \ args -> do
        expected <- mapM mkFile ["bar", "baz"]
        cliArgsWatched args `shouldBe` expected
