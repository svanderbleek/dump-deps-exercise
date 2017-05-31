{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec
  (hspec
  ,describe
  ,it
  ,shouldBe
  ,shouldSatisfy)

import System.Process
  (readProcess
  ,readProcessWithExitCode)

import System.Exit
  (ExitCode(..))

import Text.RawString.QQ
  (r)

import Deps
  (findDeps)

import Graph
  (displayDeps
  ,insertDeps
  ,emptyDeps)

import Types
  (Deps
  ,ModWithDeps(..)
  ,ModId(..))

import Data.List
  (isInfixOf)

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
        (code, _, _) <- runArgs ["src/Main.hs", "--src", "src/"]
        code `shouldBe` ExitSuccess

    describe "graph" $ do
      it "displays module dependency graphs" $ do
        let out = displayDeps simpleDeps
        out `shouldSatisfy` (isInfixOf "M1")
        out `shouldSatisfy` (isInfixOf "D1")

simpleDeps :: Deps
simpleDeps = insertDeps emptyDeps (ModWithDeps (ModId "M1") [ModId "D1"])

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
