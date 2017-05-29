{-# LANGUAGE QuasiQuotes #-}

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
        (_, out, err) <- readProcessWithExitCode "stack" ["exec", "deps"] ""
        err `shouldBe` helpErr

helpErr :: String
helpErr = [r|
Test program

Usage: Example.hs --foo INT --bar DOUBLE

Available options:
  -h,--help                Show this help text
|]
