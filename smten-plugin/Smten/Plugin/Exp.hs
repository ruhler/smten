
module Smten.Plugin.Exp (
    bindCG
  ) where

import Data.Functor

import GhcPlugins

import Smten.Plugin.CG
import Smten.Plugin.Name
import Smten.Plugin.Type

import qualified Smten.Plugin.Output.Syntax as S

bindCG :: CoreBind -> CG [S.Val]
bindCG (Rec xs) = concat <$> mapM bindCG [NonRec x v | (x, v) <- xs]
bindCG b@(NonRec var body) = do
  --lift $ putMsg (ppr b)
  body' <- expCG body
  nm <- nameCG $ varName var
  ty <- topTypeCG $ varType var
  return [S.Val nm (Just ty) body']


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

-- Case expressions are generated as follows:
--   let casef_XX = \v ->
--          case v of
--             FooA a b ... -> ...
--             FooB a b ... -> ...
--             ...
--             Foo_Error msg -> error0 msg
--             Foo_Ite {} -> flsapp casef_XX v [__iteFooA v, __iteFooB v, ...]
--             Foo_Prim {} -> ???
--             _ -> default_body
--   in case_XX x
expCG (Case x v ty ms) = do
    uniq <- lift $ getUniqueM
    let occ = mkVarOcc "casef"
        casef = mkSystemName uniq occ
    withlocals [varName v, casef] $ do
        v' <- qnameCG $ varName v
        casef' <- qnameCG casef
        x' <- expCG x
        ms' <- altsCG (varType v) ms
        let casee = S.CaseE (S.VarE v') ms'
            lame = S.LamE v' casee
            bind = S.Val casef' Nothing lame
            ine = S.AppE (S.VarE casef') x'
        return $ S.LetE [bind] ine

-- TODO: insert a call to unsafeCoerce# here?
expCG (Cast x _) = expCG x
expCG x = do
  lift $ errorMsg (text "TODO: expCG " <+> ppr x)
  return (S.VarE "???")

litCG :: Literal -> S.Literal
litCG (MachStr str) = S.StringL (unpackFS str)
litCG (MachChar c) = S.CharL c
litCG (MachInt i) = S.IntL i
litCG (MachWord i) = S.WordL i
litCG (LitInteger i _) = S.IntegerL i

altCG :: CoreAlt -> CG S.Alt
altCG (DataAlt k, xs, body) = withlocals (map varName xs) $ do
    body' <- expCG body
    xs' <- mapM (qnameCG . varName) xs
    k' <- qnameCG $ getName k
    return $ S.Alt (S.ConP k' (map S.VarP xs')) body'
altCG (LitAlt l, _, body) = do
    body' <- expCG body
    return $ S.Alt (S.LitP (litCG l)) body'

-- Foo_Error msg -> error0 msg
erraltCG :: Type -> CG S.Alt
erraltCG t = do 
  let tynm = tyConName . fst $ splitTyConApp t
  qerrnm <- qerrnmCG tynm
  addimport "Smten.Runtime.SmtenHS"
  return $ S.Alt (S.ConP qerrnm [S.VarP "msg"])
                 (S.AppE (S.VarE "Smten.Runtime.SmtenHS.error0") (S.VarE "msg"))

altsCG :: Type -> [CoreAlt] -> CG [S.Alt]
altsCG t ((DEFAULT, _, body) : xs) = do
  xs' <- altsCG t xs
  body' <- expCG body
  return $ xs' ++ [S.Alt S.wildP body']
altsCG t xs = do
  alts <- mapM altCG xs
  erralt <- erraltCG t
  return (alts ++ [erralt])

