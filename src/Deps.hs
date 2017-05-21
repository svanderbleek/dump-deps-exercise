module Deps
  (findDependencies
  ,Dependencies)
where

import Graph
  (transitiveReduction)

import Parse
  (findModules)

import Domain
  (ModuleIdentifier(..)
  ,Dependencies(..))

findDependencies
  :: FilePath -- entry point
  -> FilePath -- src directory
  -> IO Dependencies
findDependencies file src =
  do
    return (D (MI ""))
