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
  ,impsAdjc
  ,Deps(..))

import Imps
  (ModWithImps(..)
  ,ModId(..))

import Data.List
  (elemIndices)

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
      it "displays clean module dependencys" $ do
        let out = displayDeps demoDeps
        out `shouldSatisfy` (containsOnce 'A')
        out `shouldSatisfy` (containsOnce 'B')
        out `shouldSatisfy` (containsOnce 'C')

containsOnce :: Char -> String -> Bool
containsOnce c =
  (==1) . length . elemIndices c

demoDeps :: Deps
demoDeps =
  Deps
  { dps_root = demoA
  , dps_adjc = impsAdjc $
    [(ModWithImps demoA [demoB, demoC])
    ,(ModWithImps demoB [demoC])
    ,(ModWithImps demoC [])] }

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
