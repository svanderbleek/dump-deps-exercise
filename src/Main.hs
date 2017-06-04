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
  ,ParserInfo
  ,(<**>))

import Data.Semigroup
  ((<>))

import Deps
  (findDeps)

data DepsCmd
  = DepsCmd
  { root :: FilePath
  , src :: FilePath }
  deriving (Show)

main :: IO ()
main = do
  (DepsCmd root src) <- execParser cliParser
  deps <- findDeps root src
  print deps

cliParser :: ParserInfo DepsCmd
cliParser =
  info prsr desc
  where
    prsr = cmdParser <**> helper
    desc = fullDesc <>
      header "deps - a command line haskell parser" <>
      progDesc "Find dependencies of root in source"

cmdParser :: Parser DepsCmd
cmdParser =
  DepsCmd <$> arg <*> opt
  where
    arg = argument str (metavar "root" <> help "root file")
    opt = strOption (long "src" <> metavar "source" <> help "source directory")
