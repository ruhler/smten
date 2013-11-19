
{-# LANGUAGE PatternGuards #-}

module Smten.Plugin.Plugin (plugin) where

import Data.Functor
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
-- The Smten ghc-plugin compiles Smten code into Haskell code, which can then
-- be compiled using ghc into an executable. To compile the Smten code, add the
-- flag \"-fplugin=Smten.Plugin.Plugin\" when calling ghc. For example:
--  
-- > ghc --make -c -fplugin=Smten.Plugin.Plugin MySmtenProgram.hs
--  
-- For each smten module, a haskell module will be generated using the name of
-- the smten module prefixed with \"Smten.Compiled\". The generated haskell files
-- can be found in the Smten\/Compiled directory. For the main module, a
-- file called Smten\/Compiled\/Main.hs will be generated.
--  
-- After the haskell files have been generated, call ghc in ordinary fashion to
-- generate an executable:
--  
-- > ghc --make -o mysmtenprogram Smten/Compiled/Main.hs
--
-- The 'smten' executable will properly invoke ghc with the smten plugin for
-- you. The manual use of the plugin as above is not advised.
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
      debugTraceMsg $ text "Smten Plugin: translating core..."
      let modnm = moduleNameString $ moduleName (mg_module m)
      mod <- runCG modnm (moduleCG cg details)
      debugTraceMsg $ text "Smten Plugin: outputting haskell..."
      flags <- getDynFlags
      let slashes = moduleNameSlashes $ moduleName (mg_module m)
          odir = fromMaybe "." (objectDir flags)
          tgt = odir ++ "/Smten/Compiled/" ++ slashes ++ ".hs"
      liftIO $ do
          createDirectoryIfMissing True (directory tgt)
          S.renderToFile tgt mod
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
  exports <- concat <$> mapM exportCG (typeEnvElts (md_types details))
  importmods <- getimports
  let myname = moduleName (cg_module cg)
      modnm = "Smten.Compiled." ++ moduleNameString myname
      imports = [S.Import nm as | (nm, as) <- importmods]
      langs = map S.LanguagePragma [
            "DataKinds", "MagicHash", "NoImplicitPrelude",
            "RankNTypes", "ScopedTypeVariables"]
  return $ S.Module {
    S.mod_pragmas = S.HaddockHide : S.GhcOption "-fno-warn-unused-binds" : langs,
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
exportCG (ADataCon d) = do
    nm <- qconnmCG $ dataConName d
    return [S.VarExport nm]
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

