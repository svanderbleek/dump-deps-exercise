{-# LANGUAGE TupleSections #-}

module Deps
  (findDeps)
where

import Parse
  (parseModules
  ,ModWithDeps(..)
  ,ModId(..))

import Data.Graph.Inductive.Graph
  (empty
  ,insNode
  ,insNodes
  ,insEdges
  ,LEdge
  ,LNode)

import Data.Graph.Inductive.PatriciaTree
  (Gr)

import Data.Hashable
  (hash)

data ModEdge
  = ModEdge String
  deriving (Eq, Show)

findDeps :: FilePath -> FilePath -> IO (Gr ModId ModEdge)
findDeps root source =
  do
    (ModWithDeps modn deps) <- parseModules root
    return $ insEdges
      (mkEdge modn <$> deps)
      (insNodes
        (mkNode <$> deps)
        (insNode (mkNode modn) empty))

mkNode :: ModId -> LNode ModId
mkNode (ModId label) =
  (hash label, ModId label)

mkEdge :: ModId -> ModId -> LEdge ModEdge
mkEdge (ModId to) (ModId from) =
  (hash to, hash from, ModEdge "depends")
