
{-# LANGUAGE PatternGuards #-}

module Smten.Plugin.Plugin (plugin) where

import Data.Functor
import Data.List
import Data.Maybe
import System.Directory

import GhcPlugins

import Smten.Plugin.CG
import Smten.Plugin.Name
import Smten.Plugin.Type
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


bindCG :: CoreBind -> CG [S.Val]
bindCG (Rec xs) = concat <$> mapM bindCG [NonRec x v | (x, v) <- xs]
bindCG b@(NonRec var body) = do
  --lift $ putMsg (ppr b)
  body' <- expCG body
  nm <- nameCG $ varName var
  ty <- topTypeCG $ varType var
  return [S.Val nm ty body']

expCG :: CoreExpr -> CG S.Exp
expCG (Var x) = S.VarE <$> (qnameCG $ varName x)
expCG (Lit l) = return (S.LitE (litCG l))
expCG (App (Lam b body) (Type t)) = withtype b t $ expCG body
expCG (App a (Type {})) = expCG a
expCG (App a b) = do
    a' <- expCG a
    b' <- expCG b
    return $ S.AppE a' b'
expCG (Let x body) = withlocals (map varName (bindersOf x)) $ do
    x' <- bindCG x
    body' <- expCG body
    return $ S.LetE x' body'
expCG (Lam b body)
 | isTyVar b = expCG body
 | otherwise = withlocal (varName b) $ do
    b' <- qnameCG $ varName b
    body' <- expCG body
    return $ S.LamE b' body'
expCG (Case x v _ ms) = withlocal (varName v) $ do
    x' <- expCG x
    ms' <- altsCG v ms
    return $ S.CaseE x' ms'

-- TODO: insert a call to unsafeCoerce# here?
expCG (Cast x _) = expCG x
expCG x = error ("TODO: expCG " ++ renderDoc (ppr x))

litCG :: Literal -> S.Literal
litCG (MachStr str) = S.StringL (unpackFS str)
litCG (MachChar c) = S.CharL c
litCG (MachInt i) = S.IntL i
litCG (MachWord i) = S.WordL i
litCG (LitInteger i _) = S.IntegerL i
litCG l = error $ "litCG: " ++ renderDoc (ppr l)

altCG :: CoreBndr -> CoreAlt -> CG S.Alt
altCG v (DataAlt k, xs, body) = withlocals (map varName xs) $ do
    body' <- expCG body
    xs' <- mapM (qnameCG . varName) xs
    k' <- qnameCG $ getName k
    v' <- qnameCG $ varName v
    return $ S.Alt (S.AsP v' (S.ConP k' xs')) body'
altCG v (LitAlt l, _, body) = do
    body' <- expCG body
    v' <- qnameCG $ varName v
    return $ S.Alt (S.AsP v' (S.LitP (litCG l))) body'

altsCG :: Var -> [CoreAlt] -> CG [S.Alt]
altsCG v ((DEFAULT, _, body) : xs) = do
  xs' <- mapM (altCG v) xs
  v' <- qnameCG $ varName v
  body' <- expCG body
  return $ xs' ++ [S.Alt (S.VarP v') body']
altsCG v xs = mapM (altCG v) xs

renderDoc :: SDoc -> String
renderDoc d = renderWithStyle tracingDynFlags d defaultUserStyle
    
directory :: FilePath -> FilePath
directory f
  | '/' `notElem` f = ""
  | (h, t) <- break (== '/') f = h ++ "/" ++ directory (tail t)

