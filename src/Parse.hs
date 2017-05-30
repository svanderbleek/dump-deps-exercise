module Parse
  (parseModules
  ,ModWithDeps(..)
  ,ModId(..))
where

import GHC (runGhc
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
  ,ms_mod_name
  ,GhcMonad
  ,HscTarget(..)
  ,GhcLink(..)
  ,ModSummary(..)
  ,Module(..)
  ,ModuleName)

import GHC.Paths
  (libdir)

import Control.Monad.IO.Class
  (liftIO)

data ModId
  = ModId String
  deriving (Show)

data ModWithDeps
  = ModWithDeps
  { modn :: ModId
  , deps :: [ModId] }

parseModules :: FilePath -> IO ModWithDeps
parseModules file =
  runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    setSessionDynFlags $ flags { hscTarget = HscNothing, ghcLink = NoLink }
    target <- guessTarget file Nothing
    addTarget target
    found <- depanal [] False
    -- TODO convert partial function to total with error
    return $ parseModWithDeps (head found)

parseModWithDeps :: ModSummary -> ModWithDeps
parseModWithDeps ms =
  ModWithDeps
  { modn = ModId $ moduleNameString (ms_mod_name ms)
  , deps = ModId . moduleNameString . unLoc . snd <$> ms_textual_imps ms }
