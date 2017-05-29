{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Test.Hspec
  (hspec
  ,describe
  ,it
  ,shouldBe)

import System.Process
  (readProcessWithExitCode
  ,CmdSpec(..))

import Text.RawString.QQ
  (r)

main :: IO ()
main =
  hspec $ do
    describe "executable" $ do
      it "outputs help" $ do
        (_, out, err) <- runHelp
        out `shouldBe` helpErr

runHelp :: IO (_, String, String)
runHelp =
  readProcessWithExitCode "stack" ["exec", "deps", "--", "-h"] ""

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
