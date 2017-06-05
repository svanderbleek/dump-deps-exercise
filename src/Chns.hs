module Chns
  (reduceDeps
  ,mkChains
  ,maxChains
  ,Chains)
where

import Typs
  (ModId(..)
  ,Deps(..)
  ,Adjc)

import Data.Map
  (Map
  ,empty
  ,insert
  ,insertWith
  ,findWithDefault
  ,(!))

type Chain =
  [ModId]

type Chains =
  [Chain]

type MaxChains =
  Map ModId Chain

reduceDeps :: Deps -> Deps
reduceDeps (Deps root adjc) =
  Deps root . chainsAdjc $ mkChains adjc root [root]

mkChains :: Adjc -> ModId -> Chain -> Chains
mkChains a m c =
  case next of
    [] -> [c]
    _ -> foldr (++) [] $ descend <$> next
  where
    next = a ! m
    descend n = mkChains a n $ push n
    push = (:c)

chainsAdjc :: Chains -> Adjc
chainsAdjc =
  mkAdjc . maxChains

maxChains :: Chains -> MaxChains
maxChains =
  foldr maxChain empty
  where
    maxChain :: Chain -> MaxChains -> MaxChains
    maxChain v@(k:_) = insertWith max k v

mkAdjc :: MaxChains -> Adjc
mkAdjc =
  foldr unwindChain empty
  where
    unwindChain :: Chain -> Adjc -> Adjc
    unwindChain c a = foldr insertAdjacent a (adjacentPairs c)
    insertAdjacent :: (ModId, ModId) -> Adjc -> Adjc
    insertAdjacent (child, parent) a = insert parent (child : findWithDefault [] parent a) a
    adjacentPairs :: [a] -> [(a, a)]
    adjacentPairs l@(_:t) = zip l t
