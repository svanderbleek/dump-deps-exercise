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
  ,lookup
  ,foldWithKey
  ,fromList)

import Data.Maybe
  (catMaybes)

type Deps = Map ModId [ModId]

findDeps :: FilePath -> FilePath -> IO Deps
findDeps root src = do
  srcFiles <- find allFiles withHs src
  mods <- mapM parseModules (root:srcFiles)
  return $ modsDeps mods
  where
    allFiles = always
    withHs = extension ==? ".hs"

displayDeps :: ModId -> Deps -> String
displayDeps root deps =
  show root ++ show deps

modsDeps :: [ModWithDeps] -> Deps
modsDeps =
  cleanDeps . fromList . (dupAp name deps <$>)

cleanDeps :: Deps -> Deps
cleanDeps deps =
  cleanMods <$> deps
  where
    cleanMods :: [ModId] -> [ModId]
    cleanMods mods = catMaybes (includes <$> mods)
    includes :: ModId -> Maybe ModId
    includes mod = const mod <$> lookup' deps mod
    lookup' = flip Data.Map.lookup

dupAp :: (a -> b) -> (a -> d) -> (a -> (b, d))
dupAp f g =
  \a -> (f a, g a)
