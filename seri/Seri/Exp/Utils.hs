
{-# LANGUAGE PatternGuards #-}

module Seri.Exp.Utils (
    free, free',
    transformMTE, transform,
    pushFunction,
    ) where

import Data.Functor
import Data.List(nub, genericLength)

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

-- Push the function into the given argument.
-- This only makes sense, and only does anything, if the argument is a case
-- expression.
--
-- Transforms:
--   f (case x of k -> \a b ... -> y; _ -> n)
--  To:
--   case x of k -> \a b ... f y; _ -> f n
pushFunction :: Exp -> Exp -> Exp
pushFunction f (CaseE x k y n) =
  let yify :: Integer -> (Exp -> Exp) -> Exp -> Exp
      yify 0 f x = f x
      yify n f (LamE s b) = LamE s (yify (n-1) f b)

      kargs = genericLength (de_arrowsT (typeof k))
      ybody = \x -> appE f x
      y' = yify kargs ybody y
  in CaseE x k y' (appE f n)

