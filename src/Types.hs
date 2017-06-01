module Types
  (ModId(..)
  ,ModWithDeps(..)
  ,Deps)
where

import Data.Map
  (Map)

import Data.Set
  (Set)

data ModId
  = ModId String
  deriving (Eq, Ord, Show)

data ModWithDeps
  = ModWithDeps
  { modn :: ModId
  , deps :: [ModId] }

type Deps
  = Map ModId (Set ModId)
