module Main where

import GHC
  (runGhc
  ,getSessionDynFlags
  ,setSessionDynFlags
  ,hscTarget
  ,ghcLink
  ,guessTarget
  ,addTarget
  ,getTargets
  ,depanal
  ,moduleNameString
  ,moduleName
  ,unLoc
  ,GhcMonad
  ,HscTarget(..)
  ,GhcLink(..)
  ,Target(..)
  ,TargetId(..)
  ,ModSummary(..)
  ,Module(..))

import GHC.Paths
  (libdir)

import Control.Monad.IO.Class
  (liftIO)

import Data.List.NonEmpty
  (NonEmpty(..))

showTarget :: Target -> String
showTarget (Target (TargetModule m) _ _) = moduleNameString m
showTarget (Target (TargetFile f _) _ _) = f

showModSummary :: ModSummary -> String
showModSummary (ModSummary m _ _ _ _ _ _ _ _ _ _) = moduleNameString (moduleName m)

showModImps :: ModSummary -> String
showModImps (ModSummary _ _ _ _ _ _ srcimps txtimps _ _ _) =
  "srcimps:" ++ (show (length srcimps)) ++ ",txtimps:" ++ (show (length txtimps))

showModLocs :: ModSummary -> String
showModLocs (ModSummary _ _ _ _ _ _ _ txtimps _ _ _) =
  show (moduleNameString . unLoc . snd <$> txtimps)

main :: IO ()
main =
  runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    setSessionDynFlags $ flags { hscTarget = HscNothing, ghcLink = NoLink }
    target <- guessTarget "Deps.hs" Nothing
    liftIO $ print (showTarget target)
    addTarget target
    targets <- getTargets
    liftIO $ print (showTarget <$> targets)
    modules <- depanal [] False
    liftIO $ print (showModSummary <$> modules)
    liftIO $ print (showModImps <$> modules)
    liftIO $ print (showModLocs <$> modules)
    return ()

newtype ModuleIdentifier
  = MI String

data Dependencies
  = D ModuleIdentifier
  | DL ModuleIdentifier (NonEmpty Dependencies)

findDeps
  :: GhcMonad m
  => FilePath -- entry point
  -> FilePath -- src directory
  -> m Dependencies
findDeps file src = undefined

-- Transitive Reduction
--
-- Removes edges that are induced by transitivity
--
-- >>> transitiveReduction (DL (MI "A") (DL (MI "B") (D (MI "C") :| [])) :| D (MI "C"))
-- DL (MI "A") (DL (MI "B") (D (MI "C") :| [])  :| [])

transitiveReduction :: Dependencies -> Dependencies
transitiveReduction = undefined
