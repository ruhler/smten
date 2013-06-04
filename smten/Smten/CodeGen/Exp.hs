
{-# LANGUAGE PatternGuards #-}

module Smten.CodeGen.Exp (topExpCG, expCG) where

import qualified Language.Haskell.TH.Syntax as H

import Smten.Name
import Smten.Sig
import Smten.Lit
import Smten.Exp
import Smten.Dec
import Smten.Ppr
import Smten.CodeGen.CG
import Smten.CodeGen.Name
import Smten.CodeGen.Type

topExpCG :: TopExp -> CG [H.Dec]
topExpCG (TopExp ts@(TopSig n ctx t) e) = do
    sig <- topSigCG ts
    e' <- expCG e
    let val = H.FunD (nameCG n) [H.Clause [] (H.NormalB e') []]
    return [sig, val]

litCG :: Lit -> CG H.Exp
litCG l 
 | Just c <- de_charL l = return $ H.AppE (H.ConE $ qtynameCG charN) (H.LitE (H.CharL c))
 | Just i <- de_integerL l = return $ H.AppE (H.ConE $ qtynameCG integerN) (H.LitE (H.IntegerL i))
 | otherwise = error $ "todo: litCG: " ++ pretty l

expCG :: Exp -> CG H.Exp
expCG e = 
  case e of
    LitE _ l -> litCG l
    ConE _ (Sig n _) -> return $ H.ConE (qnameCG n)
    VarE _ (Sig n t) -> do
        -- Give explicit type signature to make sure there are no type
        -- ambiguities
        t' <- typeCG t
        return $ H.SigE (H.VarE (qnameCG n)) t'

    AppE _ f x -> do
        f' <- expCG f
        x' <- expCG x
        return $ H.AppE f' x'

    LamE _ (Sig n _) x -> do
        x' <- expCG x
        return $ H.LamE [H.VarP (nameCG n)] x'

    CaseE _ x (Sig kn _) y n -> do
        [x', y', n'] <- mapM expCG [x, y, n]
        return $ foldl1 H.AppE [H.VarE (qcasenmCG kn), x', y', n']

    LetE _ bs x -> do
      let f :: (Sig, Exp) -> CG H.Dec
          f (Sig n t, v) = do
            t' <- typeCG t
            v' <- expCG v
            return $ H.ValD (H.SigP (H.VarP (nameCG n)) t') (H.NormalB v') []
      ds <- mapM f bs
      x' <- expCG x
      return $ H.LetE ds x'
    

