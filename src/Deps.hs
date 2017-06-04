module Deps
  (findDeps
  ,displayDeps
  ,impsAdjc
  ,Deps(..))
where

import Imps
  (fileImps
  ,ModWithImps(..)
  ,ModId(..))

import System.FilePath.Find
  (find
  ,always
  ,extension
  ,(==?))

import Data.Map
  (Map ,lookup ,foldWithKey
  ,fromList)

import Data.Maybe
  (catMaybes)

import Control.Arrow
  ((&&&))

-- Adjacency Map
type Adjc
  = Map ModId [ModId]

data Deps
  = Deps
  { dps_root :: ModId
  , dps_adjc :: Adjc }

findDeps :: FilePath -> FilePath -> IO Deps
findDeps root src = do
  srcFiles <- find allFiles withHs src
  rootImps <- fileImps root
  srcImps <- mapM fileImps srcFiles
  return $ Deps
    { dps_root = mwi_name rootImps
    , dps_adjc = impsAdjc (rootImps:srcImps) }
  where
    allFiles = always
    withHs = extension ==? ".hs"

displayDeps :: Deps -> String
displayDeps (Deps root adjc) =
  show root ++ show adjc

impsAdjc :: [ModWithImps] -> Adjc
impsAdjc =
  cleanAdjc . fromList . (modNameImps <$>)
  where
    modNameImps = mwi_name &&& mwi_imps

cleanAdjc :: Adjc -> Adjc
cleanAdjc adjc =
  cleanMods <$> adjc
  where
    cleanMods :: [ModId] -> [ModId]
    cleanMods mods = catMaybes (includes <$> mods)
    includes :: ModId -> Maybe ModId
    includes mod = const mod <$> lookup' adjc mod
    lookup' = flip Data.Map.lookup

reduceDeps :: Deps -> Deps
reduceDeps deps =
  undefined
