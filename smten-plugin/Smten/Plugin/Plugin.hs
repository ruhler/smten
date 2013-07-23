
{-# LANGUAGE PatternGuards #-}

module Smten.Plugin.Plugin (plugin) where

import Data.Functor
import Data.List
import Data.Maybe
import System.Directory

import GhcPlugins

import Smten.Plugin.CG
import Smten.Plugin.Exp
import Smten.Plugin.TyCon
import Smten.Plugin.Annotations
import qualified Smten.Plugin.Output.Syntax as S
import qualified Smten.Plugin.Output.Ppr as S

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
 }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  reinitializeGlobals
  return (CoreDoPluginPass "Smten" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass m = do
  mod <- runCG (moduleCG m)
  isprim <- getIsPrim m 
  if isprim
     then putMsg (ppr (mg_module m) <+> text "is primitive.")
     else do
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


moduleCG :: ModGuts -> CG S.Module
moduleCG m = do
  datas <- concat <$> mapM tyconCG (mg_tcs m)
  vals <- concat <$> mapM bindCG (mg_binds m)
  importmods <- getimports
  let myname = moduleName (mg_module m)
      modnm = "Smten.Compiled." ++ moduleNameString myname
      imports = filter ((/=) modnm) . nub $ importmods
  return $ S.Module {
    S.mod_langs = ["MagicHash", "RankNTypes", "ScopedTypeVariables"],
    S.mod_name = modnm,
    S.mod_imports = imports,
    S.mod_decs = datas ++ (map S.ValD vals)
   }

directory :: FilePath -> FilePath
directory f
  | '/' `notElem` f = ""
  | (h, t) <- break (== '/') f = h ++ "/" ++ directory (tail t)

