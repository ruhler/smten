
{-# LANGUAGE PatternGuards #-}

module Smten.Plugin.Plugin (plugin) where

import Data.Functor
import Data.List
import Data.Maybe
import System.Directory

import Class
import GhcPlugins

import Smten.Plugin.CG
import Smten.Plugin.Name
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
  flags <- getDynFlags
  let slashes = moduleNameSlashes $ moduleName (mg_module m)
      odir = fromMaybe "." (objectDir flags)
      tgt = odir ++ "/Smten/Compiled/" ++ slashes ++ ".hs"
  liftIO $ do
      createDirectoryIfMissing True (directory tgt)
      writeFile tgt (S.render mod)
  return m

moduleCG :: ModGuts -> CG S.Module
moduleCG m = do
  datas <- concat <$> mapM tyconCG (mg_tcs m)
  vals <- concat <$> mapM bindCG (mg_binds m)
  importmods <- getimports
  let myname = moduleName (mg_module m)
      modnm = "Smten.Compiled." ++ moduleNameString myname
      imports = filter ((/=) modnm) . nub $ importmods
  return $ S.Module {
    S.mod_langs = ["MagicHash", "RankNTypes"],
    S.mod_name = modnm,
    S.mod_imports = imports,
    S.mod_datas = datas,
    S.mod_vals = vals
   }

-- Declare a type constructor.
tyconCG :: TyCon -> CG [S.DataD]
tyconCG t
 | Just cls <- tyConClass_maybe t
 , Just [dc] <- tyConDataCons_maybe t = do
     let mkfield :: Id -> CG S.RecField
         mkfield x = do
           t <- typeCG $ varType x
           nm <- nameCG $ varName x
           return $ S.RecField nm t
     fields <- mapM mkfield (classMethods cls)
     cn <- nameCG $ dataConName dc
     t' <- nameCG $ tyConName t
     vs <- mapM (qnameCG . varName) (tyConTyVars t)
     return [S.DataD t' vs [S.RecC cn fields]]
        
 | Just cs <- tyConDataCons_maybe t = do
     let mkcon :: DataCon -> CG S.Con
         mkcon d = do
           tys <- mapM typeCG (dataConOrigArgTys d)
           nm <- nameCG (dataConName d)
           return $ S.Con nm tys
     ks <- mapM mkcon cs
     t' <- nameCG (tyConName t)
     vs <- mapM (qnameCG . varName) (tyConTyVars t)
     return [S.DataD t' vs ks]
 | isSynTyCon t = return []
 | otherwise = error $ "tyconCG: " ++ renderDoc (ppr t)
  

bindCG :: CoreBind -> CG [S.ValD]
bindCG (Rec xs) = concat <$> mapM bindCG [NonRec x v | (x, v) <- xs]
bindCG b@(NonRec var body) = do
  --lift $ putMsg (ppr b)
  body' <- expCG body
  nm <- nameCG $ varName var
  ty <- typeCG $ varType var
  return [S.ValD nm ty body']

typeCG :: Type -> CG S.Type
typeCG t 
 | Just (tycon, args) <- splitTyConApp_maybe t = do
     k <- qnameCG (tyConName tycon)
     args' <- mapM typeCG args
     return $ S.ConAppT k args'
 | (vs@(_:_), t) <- splitForAllTys t = do
     vs' <- mapM (qnameCG . varName) vs
     t' <- typeCG t
     return $ S.ForallT vs' t'
 | Just v <- getTyVar_maybe t = S.VarT <$> qnameCG (varName v)
 | Just (a, b) <- splitAppTy_maybe t = do
     a' <- typeCG a
     b' <- typeCG b
     return $ S.AppT a' b'
 | otherwise = error ("typeCG: " ++ renderDoc (ppr t))

expCG :: CoreExpr -> CG S.Exp
expCG (Var x) = S.VarE <$> (qnameCG $ varName x)
expCG (Lit l) = return (S.LitE (litCG l))
expCG (App a (Type {})) = expCG a
expCG (App a b) = do
    a' <- expCG a
    b' <- expCG b
    return $ S.AppE a' b'
expCG (Let x body) = do
    x' <- bindCG x
    body' <- expCG body
    return $ S.LetE x' body'
expCG (Lam b body)
 | isTyVar b = expCG body
 | otherwise = do
    b' <- qnameCG $ varName b
    body' <- expCG body
    return $ S.LamE b' body'
expCG (Case x v _ ms) = do
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
altCG v (DataAlt k, xs, body) = do
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

