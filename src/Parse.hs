module Parse
  (findModules)
where

import GHC (runGhc
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

import Domain
  (ModuleIdentifier
  ,FoundModule)

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

findModules :: FilePath -> IO [FoundModule]
findModules file =
  runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    setSessionDynFlags $ flags { hscTarget = HscNothing, ghcLink = NoLink }
    target <- guessTarget file Nothing
    addTarget target
    modules <- depanal [] False
    liftIO $ print (showModSummary <$> modules)
    liftIO $ print (showModImps <$> modules)
    liftIO $ print (showModLocs <$> modules)
    return []
