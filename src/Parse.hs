module Parse
  (parseModules
  ,ModWithDeps(..)
  ,ModId(..))
where

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
  ,moduleName
  ,unLoc
  ,getLoc
  ,ms_mod_name
  ,ms_textual_imps
  ,GhcMonad
  ,HscTarget(..)
  ,GhcLink(..)
  ,ModSummary(..)
  ,Module(..)
  ,ModuleName)

import GHC.Paths
  (libdir)

import Outputable
  (ppr
  ,showSDoc)

import DriverPhases
  (hscSourceString)

import Control.Monad.IO.Class
  (liftIO)

data ModId
  = ModId String
  deriving (Eq, Ord, Show)

data ModWithDeps
  = ModWithDeps
  { file :: FilePath
  , name :: ModId
  , deps :: [ModId] }
  deriving (Show)

parseModules :: FilePath -> IO ModWithDeps
parseModules file =
  runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    setSessionDynFlags $ flags { hscTarget = HscNothing, ghcLink = NoLink }
    target <- guessTarget file Nothing
    addTarget target
    [found] <- depanal [] False
    return $ ModWithDeps
      { file = file
      , name = msModId found
      , deps = msDeps found }

msDeps :: ModSummary -> [ModId]
msDeps =
  (impModId <$>) . ms_textual_imps
  where
    impModId = ModId . moduleNameString . unLoc . snd

msModId :: ModSummary -> ModId
msModId =
  ModId . moduleNameString . ms_mod_name
