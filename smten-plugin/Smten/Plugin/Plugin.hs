
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
  body <- vcat <$> mapM bindCG (mg_binds m)
  importmods <- getimports
  let myname = moduleName (mg_module m)
      modnm = moduleNameString myname
      importnms = filter ((/=) myname) . nub $ importmods
      imports = vcat [text "import qualified" <+> ppr n | n <- importnms]
  return $
    text "{-# LANGUAGE MagicHash #-}" $+$
    text "module" <+> text modnm <+> text "where" $+$
    imports $+$
    body

bindCG :: CoreBind -> CG SDoc
bindCG (Rec xs) = vcat <$> mapM bindCG [NonRec x v | (x, v) <- xs]
bindCG b@(NonRec var body) = do
  body' <- expCG body
  nm <- nameCG $ varName var
  ty <- typeCG $ varType var
  return $
    nm <+> text "::" <+> ty $+$
    nm <+> text "=" <+> body'

typeCG :: Type -> CG SDoc
typeCG t = return (ppr t)

expCG :: CoreExpr -> CG SDoc
expCG (Var x)
 | "GHC.Tuple.()" == renderDoc (ppr x) = return $ text "()"
 | otherwise = qnameCG $ varName x
expCG (Lit (MachStr str)) = return $ text (show (unpackFS str)) <> text "#"
expCG (App a (Type {})) = expCG a
expCG (App a (Var x)) | isDictId x = expCG a
expCG (App a b) = do
    a' <- expCG a
    b' <- expCG b
    return $ parens (a' <+> b')
expCG (Lam b body) = do
    b' <- qnameCG $ varName b
    body' <- expCG body
    return $ parens (text "\\" <+> b' <+> text "->" <+> body')
expCG (Case x v _ ms) = do
    x' <- expCG x
    ms' <- mapM (altCG v) ms
    return $ text "case" <+> x' <+> text "of" <+> braces (vcat ms')
expCG x = error ("TODO: expCG " ++ renderDoc (ppr x))

altCG :: CoreBndr -> CoreAlt -> CG SDoc
altCG v (DataAlt k, xs, body) = do
    body' <- expCG body
    xs' <- sep <$> mapM (nameCG . varName) xs
    k' <- qnameCG $ getName k
    return $ k' <+> xs' <+> text "->" <+> body' <+> semi

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

