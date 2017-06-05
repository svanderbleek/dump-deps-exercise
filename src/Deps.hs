module Deps
  (findDeps
  ,displayDeps
  ,impsAdjc
  ,reduceDeps
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

import Debug.Trace

-- Adjacency Map
type Adjc
  = Map ModId [ModId]

data Deps
  = Deps
  { dps_root :: ModId
  , dps_adjc :: Adjc }
  deriving (Show)

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

-- A Chain is a Stack of ModIds
type Chain
  = [ModId]

-- Chains are a map of ModIds
-- to all Chains ending with that ModId
type Chains
  = Map ModId [[ModId]]

type RChains
  = Map ModId [ModId]

dt a =
  traceShow a a

dtp :: Show a => String -> a -> a
dtp p a =
  trace (p ++ show a) a

reduceDeps :: Deps -> Deps
reduceDeps deps@(Deps root _) =
  Deps root . dtp "tA" . transposeAdjc . dtp "cA" . chainsAdjc . dtp "dC" . depsChains $ dt deps

depsChains :: Deps -> Chains
depsChains (Deps root adjc) =
  mkChains init adjc root
  where
    init = singleton root [[root]]

mkChains :: Chains -> Adjc -> ModId -> Chains
mkChains acc adj cur =
  let next = adj ! cur in
  let adds = dtp "ad" [n:c | c <- dtp "c" (acc ! cur), n <- dtp "n" next] in
  let acc' = dtp "a'" (foldr hinsert acc adds) in
  dtp "uW" (unionsWith (++) (dtp "mC" (mkChains acc' adj <$> next)))
  where
    hinsert :: [ModId] -> Chains -> Chains
    hinsert v@(k:_) = insertWith (++) k [v]

chainsAdjc :: Chains -> Adjc
chainsAdjc =
  (foldr max [] <$>)

transposeAdjc :: Adjc -> Adjc
transposeAdjc =
  foldr tinsert empty
  where
    tinsert :: [ModId] -> Adjc -> Adjc
    tinsert (_:v:k:[]) = insertWith (++) k [v]

mlookup :: Ord k => Map k v -> k -> Maybe v
mlookup =
  flip Data.Map.lookup
