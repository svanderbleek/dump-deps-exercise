module Deps
  (findDeps
  ,displayDeps
  ,impsAdjc
  ,reduceDeps)
where

import Imps
  (fileImps)

import Typs
  (ModWithImps(..)
  ,ModId(..)
  ,Deps(..)
  ,Adjc)

import Chns
  (reduceDeps)

import System.FilePath.Find
  (find
  ,always
  ,extension
  ,(==?))

import Data.Map
  (Map
  ,lookup
  ,empty
  ,foldWithKey
  ,fromList
  ,insertWith
  ,singleton
  ,unionsWith
  ,(!))

import Data.Maybe
  (catMaybes)

import Control.Arrow
  ((&&&))

import Data.Map
  (Map
  ,lookup
  ,empty
  ,foldWithKey
  ,fromList
  ,insertWith
  ,singleton
  ,unionsWith
  ,(!))
import Control.Monad.State
  (State
  ,evalState
  ,get
  ,put)

findDeps :: FilePath -> FilePath -> IO Deps
findDeps root src = do
  srcFiles <- find allFiles withHs src
  rootImps <- fileImps root
  srcImps <- mapM fileImps srcFiles
  return $ reduceDeps Deps
    { dps_root = mwi_name rootImps
    , dps_adjc = impsAdjc (rootImps:srcImps) }
  where
    allFiles = always
    withHs = extension ==? ".hs"

displayDeps :: Deps -> String
displayDeps (Deps root adjc) =
  show adjc

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
    includes mod = const mod <$> mlookup adjc mod

mlookup :: Ord k => Map k v -> k -> Maybe v
mlookup =
  flip Data.Map.lookup
