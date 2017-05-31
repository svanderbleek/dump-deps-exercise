{-# LANGUAGE TupleSections #-}

module Deps
  (findDeps)
where

import Parse
  (parseModules)

import Graph
  (emptyDeps
  ,insertDeps
  ,transReduc)

import System.FilePath.Find
  (find
  ,always
  ,extension
  ,(==?))

import Types
  (ModWithDeps(..)
  ,Deps)

findDeps :: FilePath -> FilePath -> IO Deps
findDeps root source =
  find allFiles withHs source >>= runFindDeps root
  where
    allFiles = always
    withHs = extension ==? ".hs"

runFindDeps :: FilePath -> [FilePath] -> IO Deps
runFindDeps root sourceFiles =
  do
    deps <- parseModules root
    return $ insertDeps emptyDeps deps
