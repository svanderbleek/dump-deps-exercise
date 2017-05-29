{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec
  (hspec
  ,describe
  ,it
  ,shouldBe)

import System.Process
  (readProcess
  ,readProcessWithExitCode)

import System.Exit
  (ExitCode(..))

import Text.RawString.QQ
  (r)

import Deps
  (findDeps)

main :: IO ()
main =
  hspec $ do
    describe "executable" $ do
      it "outputs help" $ do
        out <- runHelp
        out `shouldBe` helpErr

      it "requires root and source" $ do
        (code, _, _) <- runArgs ["bad", "args"]
        code `shouldBe` ExitFailure 1

      it "parses root and source" $ do
        (code, _, _) <- runArgs ["root", "--src", "source"]
        code `shouldBe` ExitSuccess

    describe "parse" $ do
      it "finds dependencies of root" $ do
        deps <- findDeps "src/Main.hs" ""
        deps `shouldBe` ["Prelude", "Data.Semigroup", "Options.Applicative"]

runHelp :: IO String
runHelp =
  readProcess "stack" ["exec", "deps", "--", "-h"] ""

runArgs :: [String] -> IO (ExitCode, String, String)
runArgs args =
  readProcessWithExitCode "stack" (["exec", "deps", "--"] ++ args) ""

helpErr :: String
helpErr = unlines . tail . lines $ [r|
deps - a command line haskell parser

Usage: deps root --src source
  Find dependencies of root in source

Available options:
  root                     root file
  --src source             source directory
  -h,--help                Show this help text
|]
