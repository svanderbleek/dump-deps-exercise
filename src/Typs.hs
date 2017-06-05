module Typs
  (Deps(..)
  ,Adjc
  ,ModId(..)
  ,ModWithImps(..))
where

import Data.Map
  (Map)

data ModId
  = ModId String
  deriving (Eq, Ord, Show)

data ModWithImps
  = ModWithImps
  { mwi_name :: ModId
  , mwi_imps :: [ModId] }
  deriving (Show)

-- Map Node to Adjacent
type Adjc
  = Map ModId [ModId]

data Deps
  = Deps
  { dps_root :: ModId
  , dps_adjc :: Adjc }
  deriving (Show)
