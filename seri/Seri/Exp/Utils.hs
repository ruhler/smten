
{-# LANGUAGE PatternGuards #-}

module Seri.Exp.Utils (
    free, free',
    transformMTE, transform, simplify, substitute,
    impliedByTrue, impliedByFalse,
    ) where

import Data.Functor
import Data.List(nub)

import Seri.Name
import Seri.Sig
import Seri.Type
import Seri.Exp.Exp
import Seri.Exp.Sugar

-- | Return a list of the free variables in the given expression.
free :: Exp -> [Sig]
free =
  let free' :: [Name] -> Exp -> [Sig]
      free' _ (LitE {}) = []
      free' _ (ConE {}) = []
      free' bound (VarE (Sig n _)) | n `elem` bound = []
      free' _ (VarE s) = [s]
      free' bound (AppE a b) = free' bound a ++ free' bound b
      free' bound (LamE (Sig n _) b) = free' (n:bound) b
      free' bound (CaseE x k y n) = free' bound x ++ free' bound y ++ free' bound n
  in nub . free' []

free' :: Exp -> [Name]
free' e = [n | Sig n _ <- free e]

-- Perform a monadic transformation on all the types appearing in the given
-- expression
transformMTE :: (Functor m, Monad m) => (Type -> m Type) -> Exp -> m Exp
transformMTE f e =
  let me = transformMTE f
  in case e of
       LitE {} -> return e
       ConE (Sig n t) -> (ConE . Sig n) <$> f t
       VarE (Sig n t) -> (VarE . Sig n) <$> f t
       AppE a b -> do
            [a', b'] <- mapM me [a, b]
            return $ AppE a' b'
       LamE (Sig n t) b -> do
          t' <- f t
          b' <- me b
          return $ LamE (Sig n t') b'
       CaseE x (Sig kn kt) y n -> do
         kt' <- f kt
         [x', y', n'] <- mapM me [x, y, n]
         return $ CaseE x' (Sig kn kt') y' n'

instance Assign Exp where
   assignl f e =
    let me = assignl f 
        mt = assignl f
    in case e of
         LitE {} -> e
         ConE (Sig n t) -> ConE (Sig n (mt t))
         VarE (Sig n t) -> VarE (Sig n (mt t))
         AppE a b -> AppE (me a) (me b)
         LamE (Sig n t) b -> LamE (Sig n (mt t)) (me b)
         CaseE x (Sig kn kt) y n -> CaseE (me x) (Sig kn (mt kt)) (me y) (me n)

-- Perform a generic transformation on an expression.
-- Applies the given function to each subexpression. Any matching
-- subexpression is replaced with the returned value, otherwise it continues
-- to recurse.
transform :: (Exp -> Maybe Exp) -> Exp -> Exp
transform g e | Just v <- g e = v
transform g e =
  let me = transform g
  in case e of
       LitE {} -> e
       ConE {} -> e
       VarE {} -> e 
       AppE f x -> AppE (me f) (me x)
       LamE s f -> LamE s (me f)
       CaseE x k y d -> CaseE (me x) k (me y) (me d)

substitute :: (Name -> Maybe Exp) -> Exp -> Exp
substitute f =
  let g (VarE (Sig n _)) = f n
      g _ = Nothing
  in transform g

simplify :: Exp -> Exp
simplify e =
  let me = simplify
  in case e of
        LitE {} -> e
        ConE {} -> e
        VarE {} -> e
        AppE f x -> appE (me f) (me x)
        LamE s f -> lamE s (me f)
        CaseE x k y d -> caseE (me x) k (me y) (me d)

impliedByTrue :: Exp -> [(Name, Exp)]
impliedByTrue e
  | VarE (Sig n _) <- e = [(n, trueE)]
  | Just (a, b) <- de_andE e = impliedByTrue a ++ impliedByTrue b
  | Just x <- de_notE e = impliedByFalse x
  | otherwise = []

impliedByFalse :: Exp -> [(Name, Exp)]
impliedByFalse e
  | VarE (Sig n _) <- e = [(n, falseE)]
  | Just (a, b) <- de_orE e = impliedByFalse a ++ impliedByFalse b
  | Just x <- de_notE e = impliedByTrue x
  | otherwise = []

