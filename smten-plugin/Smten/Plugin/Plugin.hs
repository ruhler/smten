
{-# LANGUAGE PatternGuards #-}

module Smten.Plugin.Plugin (plugin) where

import Data.Functor
import Data.List
import System.Directory

import GhcPlugins

import Smten.Plugin.CG
import Smten.Plugin.Name

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
  doc <- runCG (moduleCG m)
  let tgt = targetFile (mg_module m)
  liftIO $ do
      createDirectoryIfMissing True (directory tgt)
      writeFile tgt (renderDoc doc)
  return m

moduleCG :: ModGuts -> CG SDoc
moduleCG m = do
  tycons <- vcat <$> mapM tyconCG (mg_tcs m)
  body <- vcat <$> mapM bindCG (mg_binds m)
  importmods <- getimports
  let myname = moduleName (mg_module m)
      modnm = moduleNameString myname
      importnms = filter ((/=) myname) . nub $ importmods
      imports = vcat [text "import qualified" <+> ppr n <+> semi | n <- importnms]
  return $
    text "{-# LANGUAGE MagicHash #-}" $+$
    text "{-# LANGUAGE ScopedTypeVariables #-}" $+$
    text "module" <+> text modnm <+> text "where" <+> text "{" $+$
    imports $+$
    tycons $+$
    body $+$
    text "}"

-- Declare a type constructor.
tyconCG :: TyCon -> CG SDoc
tyconCG t
 | Just cs <- tyConDataCons_maybe t = do
     let mkcon :: DataCon -> CG SDoc
         mkcon d = do
           tys <- mapM typeCG (dataConOrigArgTys d)
           nm <- nameCG (dataConName d)
           return $ nm <+> sep tys
     ks <- mapM mkcon cs
     t' <- nameCG (tyConName t)
     vs <- mapM (qnameCG . varName) (tyConTyVars t)
     return $ text "data" <+> t' <+> sep vs <+> text "=" <+> vcat (punctuate (text "|") ks) <+> semi
 | isSynTyCon t = return empty
 | otherwise = error $ "tyconCG: " ++ renderDoc (ppr t)
  

bindCG :: CoreBind -> CG SDoc
bindCG (Rec xs) = vcat <$> mapM bindCG [NonRec x v | (x, v) <- xs]
bindCG (NonRec x _) | isDictId x = return empty
bindCG b@(NonRec var body) = do
  body' <- expCG body
  nm <- nameCG $ varName var
  ty <- typeCG $ varType var
  return $
    nm <+> text "::" <+> ty <+> semi $+$
    nm <+> text "=" <+> body' <+> semi

typeCG :: Type -> CG SDoc
typeCG t 
 | Just (tycon, args) <- splitTyConApp_maybe t = do
     k <- qnameCG (tyConName tycon)
     args' <- mapM typeCG args
     return $ parens (sep (k : args'))
 | Just (a, b) <- splitFunTy_maybe t = do
     a' <- typeCG a
     b' <- typeCG b
     return $ parens (a' <+> text "->" <+> b')
 | (vs@(_:_), t) <- splitForAllTys t = do
     vs' <- mapM (qnameCG . varName) vs
     t' <- typeCG t
     return $ text "forall" <+> (sep vs') <+> text "." <+> t'
 | Just v <- getTyVar_maybe t = qnameCG (varName v)
 | otherwise = error ("typeCG: " ++ renderDoc (ppr t))

expCG :: CoreExpr -> CG SDoc
expCG (Var x) = qnameCG $ varName x
expCG (Lit (MachStr str)) = return $ text (show (unpackFS str)) <> text "#"
expCG (Lit (LitInteger i _)) = return $ integer i
expCG (Lit l) = return $ ppr l
expCG (App a (Type {})) = expCG a
expCG (App a (Var x)) | isDictId x = expCG a
expCG (App a b) = do
    a' <- expCG a
    b' <- expCG b
    return $ parens (a' <+> b')
expCG (Let x body) = do
    x' <- bindCG x
    body' <- expCG body
    return $ parens (text "let" <+> braces x' <+> text "in" <+> body')
expCG (Lam b body) = do
    b' <- qnameCG $ varName b
    body' <- expCG body
    return $ parens (text "\\" <+> b' <+> text "->" <+> body')
expCG (Case x v _ ms) = do
    x' <- expCG x
    ms' <- altsCG v ms
    return $ parens (text "case" <+> x' <+> text "of" <+> braces ms')
expCG x = error ("TODO: expCG " ++ renderDoc (ppr x))

altCG :: CoreBndr -> CoreAlt -> CG SDoc
altCG v (DataAlt k, xs, body) = do
    body' <- expCG body
    xs' <- sep <$> mapM (qnameCG . varName) xs
    k' <- qnameCG $ getName k
    v' <- qnameCG $ varName v
    return $ v' <> text "@" <> parens (k' <+> xs') <+> text "->" <+> body' <+> semi
altCG v (LitAlt l, _, body) = do
    body' <- expCG body
    v' <- qnameCG $ varName v
    return $ v' <> text "@" <> parens (ppr l) <+> text "->" <+> body' <+> semi
    

altsCG :: Var -> [CoreAlt] -> CG SDoc
altsCG v ((DEFAULT, _, body) : xs) = do
  xs' <- vcat <$> mapM (altCG v) xs
  v' <- qnameCG $ varName v
  body' <- expCG body
  return $ xs' $+$ (v' <+> text "->" <+> body' <+> semi)
altsCG v xs = vcat <$> mapM (altCG v) xs

targetFile :: Module -> FilePath
targetFile m =
  let slashes = moduleNameSlashes $ moduleName m
  in "build/test/" ++ slashes ++ ".hs"

renderDoc :: SDoc -> String
renderDoc d = renderWithStyle tracingDynFlags d defaultUserStyle
    
directory :: FilePath -> FilePath
directory f
  | '/' `notElem` f = ""
  | (h, t) <- break (== '/') f = h ++ "/" ++ directory (tail t)

