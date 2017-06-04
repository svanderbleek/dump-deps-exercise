module Deps
  (findDeps
  ,displayDeps
  ,modsDeps)
where

import Parse
  (parseModules
  ,ModWithDeps(name, deps)
  ,ModId(..))

import System.FilePath.Find
  (find
  ,always
  ,extension
  ,(==?))

import Data.Map
  (Map
  ,fromList)

type Deps = Map ModId [ModId]

findDeps :: FilePath -> FilePath -> IO [ModWithDeps]
findDeps root src = do
  srcFiles <- find allFiles withHs src
  mapM parseModules (root:srcFiles)
  where
    allFiles = always
    withHs = extension ==? ".hs"

displayDeps :: ModId -> Deps -> String
displayDeps root deps =
  show root ++ show deps

modsDeps :: [ModWithDeps] -> Deps
modsDeps =
  fromList . (dupAp name deps <$>)

dupAp :: (a -> b) -> (a -> d) -> (a -> (b, d))
dupAp f g =
  \a -> (f a, g a)
