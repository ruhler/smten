
{-# LANGUAGE PatternGuards #-}

module Smten.Plugin.Plugin (plugin) where

import Data.Functor
import Data.List
import Data.Maybe
import System.Directory

import GhcPlugins
import TidyPgm

import Smten.Plugin.CG
import Smten.Plugin.Exp
import Smten.Plugin.Name
import Smten.Plugin.TyCon
import Smten.Plugin.Annotations
import qualified Smten.Plugin.Output.Syntax as S
import qualified Smten.Plugin.Output.Ppr as S

-- | The smten plugin for ghc.
plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
 }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  reinitializeGlobals
  return (todo ++ [CoreDoPluginPass "Smten" pass])

pass :: ModGuts -> CoreM ModGuts
pass m = do
  isprim <- getIsPrim m 
  if isprim
     then putMsg (ppr (mg_module m) <+> text "is primitive.")
     else do
      hsc_env <- getHscEnv
      (cg, details) <- liftIO $ tidyProgram hsc_env m
      mod <- runCG (moduleCG cg details)
      flags <- getDynFlags
      let slashes = moduleNameSlashes $ moduleName (mg_module m)
          odir = fromMaybe "." (objectDir flags)
          tgt = odir ++ "/Smten/Compiled/" ++ slashes ++ ".hs"
      liftIO $ do
          createDirectoryIfMissing True (directory tgt)
          writeFile tgt (S.render mod)
  return m

getIsPrim :: ModGuts -> CoreM Bool
getIsPrim m = do
    anns <- getAnnotations deserializeWithData m
    let elems :: [PrimitiveModule]
        elems = lookupWithDefaultUFM anns [] ((ModuleTarget (mg_module m) :: CoreAnnTarget))
    return (not (null elems))


moduleCG :: CgGuts -> ModDetails -> CG S.Module
moduleCG cg details = do
  datas <- concat <$> mapM tyconCG (cg_tycons cg)
  vals <- concat <$> mapM bindCG (cg_binds cg)
  importmods <- getimports
  exports <- concat <$> mapM exportCG (typeEnvElts (md_types details))
  let myname = moduleName (cg_module cg)
      modnm = "Smten.Compiled." ++ moduleNameString myname
      imports = filter ((/=) modnm) . nub $ importmods
      langs = map S.LanguagePragma [
            "DataKinds", "MagicHash", "NoImplicitPrelude",
            "RankNTypes", "ScopedTypeVariables"]
  return $ S.Module {
    S.mod_pragmas = S.HaddockHide : langs,
    S.mod_name = modnm,
    S.mod_exports = exports,
    S.mod_imports = imports,
    S.mod_decs = datas ++ (map S.ValD vals)
   }

exportCG :: TyThing -> CG [S.Export]
exportCG (AnId x)
  | Just _ <- isDataConId_maybe x = return []
  | otherwise = do
    nm <- qnameCG (varName x)
    return [S.VarExport nm]
exportCG (ADataCon _) = return []
exportCG (ATyCon t)
  | isSynTyCon t = return []
  | otherwise = do
    nm <- qtynameCG (tyConName t)
    return [S.TyConExport nm]
exportCG (ACoAxiom _) = return []

directory :: FilePath -> FilePath
directory f
  | '/' `notElem` f = ""
  | (h, t) <- break (== '/') f = h ++ "/" ++ directory (tail t)

