module Types where

import Data.Graph.Inductive.PatriciaTree
  (Gr)

data ModId
  = ModId String
  deriving (Eq, Show)

data ModEdge
  = ModEdge String
  deriving (Eq, Show)

data ModWithDeps
  = ModWithDeps
  { modn :: ModId
  , deps :: [ModId] }

type Deps = Gr ModId ModEdge
