{-# LANGUAGE TupleSections #-}

module Deps
  (findDeps)
where

import Parse
  (parseModules
  ,ModWithDeps(..))

import System.FilePath.Find
  (find
  ,always
  ,extension
  ,(==?))

findDeps :: FilePath -> FilePath -> IO [ModWithDeps]
findDeps root src = do
  srcFiles <- find allFiles withHs src
  mapM parseModules (root:srcFiles)
  where
    allFiles = always
    withHs = extension ==? ".hs"
