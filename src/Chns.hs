module Chns
  (reduceDeps)
where

import Typs
  (ModId
  ,Deps(..)
  ,Adjc)

import Data.Map
  (Map
  ,empty
  ,insertWith
  ,insert
  ,singleton
  ,(!))

import Control.Monad.State
  (State
  ,evalState
  ,get
  ,put)

import Control.Monad.Loops
  (whileM_)

-- Stack of ModId
type Chain
  = [ModId]

-- Map ModId to terminal Chains
type Chains
  = Map ModId [Chain]

-- Reduce to single terminal Chain
type RChains
  = Map ModId Chain

-- Build Chains from Deps, convert to Adj
reduceDeps :: Deps -> Deps
reduceDeps deps@(Deps root _) =
  Deps root . reduce $ deps
  where
    reduce = chainsAdjc . depsRChains

depsRChains :: Deps -> RChains
depsRChains (Deps root adjc) =
  chainsRChains $ foldr (+:) (term root) adjc
  where
    term :: ModId -> Chains
    term m = singleton m [[m]]
    (+:) :: [ModId] -> Chains -> Chains
    ms +: cs = foldr (+::) (adjc ! m)
    (+::) :: ModId -> Chain -> Chains
    m +:: cs = undefined
    (+:::) :: ModId -> Chain -> Chains
    m +::: c = undefined

chainsRChains :: Chains -> RChains
chainsRChains =
  foldr (rinsert . maxChain) empty
  where
    maxChain :: [Chain] -> Chain
    maxChain = foldr max []
    rinsert :: Chain -> RChains -> RChains
    rinsert v@(k:_) = insert k v

chainsAdjc :: RChains -> Adjc
chainsAdjc =
  foldr combine empty
  where
    combine :: Chain -> Adjc -> Adjc
    combine (k:[]) = insert k []
    combine (k:v:_) = insert k [v]
