module Imps
  (fileImps)
where

import Typs
  (ModWithImps(..)
  ,ModId(..))

import GHC
  (runGhc
  ,getSessionDynFlags
  ,setSessionDynFlags
  ,hscTarget
  ,ghcLink
  ,guessTarget
  ,addTarget
  ,depanal
  ,moduleNameString
  ,unLoc
  ,ms_mod_name
  ,ms_textual_imps
  ,HscTarget(..)
  ,GhcLink(..)
  ,ModSummary(..))

import GHC.Paths
  (libdir)

import Control.Monad.IO.Class
  (liftIO)

fileImps :: FilePath -> IO ModWithImps
fileImps file =
  runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    setSessionDynFlags $ flags { hscTarget = HscNothing, ghcLink = NoLink }
    target <- guessTarget file Nothing
    addTarget target
    [found] <- depanal [] False
    return $ ModWithImps
      { mwi_name = msModId found
      , mwi_imps = msImps found }

msImps :: ModSummary -> [ModId]
msImps =
  (impModId <$>) . ms_textual_imps
  where
    impModId = ModId . moduleNameString . unLoc . snd

msModId :: ModSummary -> ModId
msModId =
  ModId . moduleNameString . ms_mod_name
