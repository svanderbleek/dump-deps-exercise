module Graph
  (emptyDeps
  ,insertDeps
  ,displayDeps
  ,transReduc)
where

import Types
  (ModId(..)
  ,ModEdge
  ,ModWithDeps(..)
  ,Deps)

import Data.Graph.Inductive.Graph
  (empty
  ,insNode
  ,insNodes
  ,insEdges
  ,prettify
  ,LEdge
  ,LNode)

emptyDeps :: Deps
emptyDeps =
  empty

insertDeps :: Deps -> ModWithDeps -> Deps
insertDeps g (ModWithDeps modn deps) = undefined

transReduc :: Deps -> Deps
transReduc = undefined

displayDeps :: Deps -> String
displayDeps = prettify
