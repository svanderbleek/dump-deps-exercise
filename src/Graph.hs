module Graph
  (emptyDeps
  ,insertDeps
  ,transReduc)
where

import Types
  (ModId
  ,ModEdge
  ,ModWithDeps(..)
  ,Deps)

import Data.Graph.Inductive.Graph
  (empty
  ,insNode
  ,insNodes
  ,insEdges
  ,LEdge
  ,LNode)

emptyDeps :: Deps
emptyDeps =
  empty

insertDeps :: Deps -> ModWithDeps -> Deps
insertDeps g (ModWithDeps modn deps) =
  insNode (mkNode g modn) g
  where
    root = mkNode g modn
    edges = mkNode

transReduc :: Deps -> Deps
transReduc = undefined

insEdges =
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

