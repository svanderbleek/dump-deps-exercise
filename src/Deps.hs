module Deps
  (findDeps)
where

import Parse
  (parseModules)

findDeps
  :: FilePath -- entry point
  -> FilePath -- src directory
  -> IO [String]
findDeps file src =
  parseModules file
