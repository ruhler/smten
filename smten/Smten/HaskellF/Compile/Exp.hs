
{-# LANGUAGE PatternGuards #-}

module Smten.HaskellF.Compile.Exp (
    hsExp
    ) where

import qualified Language.Haskell.TH.Syntax as H

import Smten.Name
import Smten.Lit
import Smten.Sig
import Smten.Exp
import Smten.HaskellF.Compile.HF
import Smten.HaskellF.Compile.Name
import Smten.HaskellF.Compile.Type

hsLit :: Lit -> H.Exp
hsLit l
 | Just i <- de_integerL l = H.AppE (H.VarE (H.mkName "Smten.HaskellF.HaskellF.integerHF")) (H.LitE (H.IntegerL i))
 | Just c <- de_charL l = H.AppE (H.VarE (H.mkName "Smten.HaskellF.HaskellF.smtenHF")) (H.LitE (H.CharL c))

hsExp :: Exp -> HF H.Exp

-- String literals:
-- TODO: the template haskell pretty printer doesn't print strings correctly
-- if they contain newlines, thus, we can't print those as string literals.
-- When they fix the template haskell pretty printer, that special case should
-- be removed here.
hsExp e
  | Just str <- de_stringE e
  , '\n' `notElem` str
    = return $ H.AppE (H.VarE (H.mkName "Smten.HaskellF.HaskellF.smtenHF")) (H.LitE (H.StringL str))

hsExp (LitE _ l) = return (hsLit l)
hsExp (ConE _ (Sig n _)) = return $ H.VarE (qconnm n)
hsExp (VarE _ (Sig n t)) = do
    -- Give explicit type signature to make sure there are no type ambiguities
    ht <- hsType t
    return $ H.SigE (H.VarE (hsqName n)) ht

hsExp (AppE _ (LamE _ (Sig n _) b) x) = do
    b' <- hsExp b
    x' <- hsExp x
    return $ H.AppE (H.LamE [H.VarP (hsName n)] b') x'

hsExp (AppE _ f x) = do
    f' <- hsExp f
    x' <- hsExp x
    return $ H.AppE (H.AppE (H.VarE (H.mkName "Smten.HaskellF.HaskellF.applyHF")) f') x'

hsExp (LamE _ (Sig n _) x) = do
    x' <- hsExp x
    return $ foldl1 H.AppE [H.VarE (H.mkName "Smten.HaskellF.HaskellF.lamHF"), H.LitE (H.StringL (unname n)), H.LamE [H.VarP (hsName n)] x']

-- case x of
--    K -> y
--    _ -> n
--
-- Translates to:  __caseK x y n
hsExp (CaseE _ x (Sig kn kt) y n) = do
    [x', y', n'] <- mapM hsExp [x, y, n]
    return $ foldl1 H.AppE [H.VarE (qcasenm kn), x', y', n']

hsExp (LetE _ bs x) = do
  let f :: (Sig, Exp) -> HF H.Dec
      f (Sig n t, v) = do
        ht <- hsType t
        v' <- hsExp v
        return $ H.ValD (H.SigP (H.VarP (hsName n)) ht) (H.NormalB v') []
  ds <- mapM f bs
  x' <- hsExp x
  return $ H.LetE ds x'
        

