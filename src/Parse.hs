module Parse
  (parseModules)
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

parseModules :: FilePath -> IO [String]
parseModules file =
  runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    setSessionDynFlags $ flags { hscTarget = HscNothing, ghcLink = NoLink }
    target <- guessTarget file Nothing
    addTarget target
    found <- depanal [] False
    return $ (moduleNameString . unLoc . snd) <$> (found >>= ms_textual_imps)
