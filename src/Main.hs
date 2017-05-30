module Main where

import Options.Applicative
  (execParser
  ,info
  ,helper
  ,fullDesc
  ,header
  ,progDesc
  ,str
  ,strOption
  ,long
  ,metavar
  ,help
  ,argument
  ,Parser
  ,(<**>))

import Data.Semigroup
  ((<>))

import Deps
  (findDeps)

import Data.Graph.Inductive.Graph
  (prettyPrint)

data DepsCmd
  = DepsCmd FilePath FilePath
  deriving (Show)

main :: IO ()
main =
  do
    (DepsCmd root source) <- execParser progParser
    deps <- findDeps root source
    prettyPrint deps

progParser =
  info
    (cmdParser <**> helper)
    (fullDesc <>
      header "deps - a command line haskell parser" <>
      progDesc "Find dependencies of root in source")

cmdParser :: Parser DepsCmd
cmdParser =
  DepsCmd <$>
    argument str (metavar "root" <> help "root file") <*>
    strOption (long "src" <> metavar "source" <> help "source directory")
