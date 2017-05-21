module Domain
  (FoundModule(..)
  ,ModuleIdentifier(..)
  ,Dependencies(..))
where

import Data.List.NonEmpty
  (NonEmpty)

newtype ModuleIdentifier
  = MI String

data FoundModule
  = FM ModuleIdentifier FilePath

data Dependencies
  = D ModuleIdentifier
  | DL ModuleIdentifier (NonEmpty Dependencies)
