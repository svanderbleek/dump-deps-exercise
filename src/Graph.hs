module Graph
  (emptyDeps
  ,insertDeps
  ,displayDeps
  ,transReduc)
where

import Types
  (ModId(..)
  ,ModWithDeps(..)
  ,Deps)

import Data.Map
  (empty
  ,insertWith)

import Data.Set
  (fromList
  ,union)

emptyDeps :: Deps
emptyDeps =
  empty

insertDeps :: Deps -> ModWithDeps -> Deps
insertDeps g (ModWithDeps modn deps) =
  insertWith union modn (fromList deps) g

transReduc :: Deps -> Deps
transReduc =
  undefined

displayDeps :: Deps -> String
displayDeps =
  show
