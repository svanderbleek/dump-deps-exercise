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
  (findDeps
  ,displayDeps
  ,modsDeps)

import Parse
  (ModWithDeps(..)
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

    describe "deps" $ do
      it "displays module dependencys" $ do
        let out = displayDeps demoA (modsDeps demoDeps)
        out `shouldSatisfy` (isInfixOf "A")
        out `shouldSatisfy` (isInfixOf "B")
        out `shouldSatisfy` (isInfixOf "C")

demoDeps :: [ModWithDeps]
demoDeps =
  [(ModWithDeps "" demoA [demoB, demoC])
  ,(ModWithDeps "" demoB [demoC])
  ,(ModWithDeps "" demoC [])]

demoA :: ModId
demoA =
  ModId "A"

demoB :: ModId
demoB =
  ModId "B"

demoC :: ModId
demoC =
  ModId "C"

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
