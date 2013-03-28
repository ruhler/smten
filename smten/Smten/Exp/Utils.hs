
{-# LANGUAGE PatternGuards #-}

module Smten.Exp.Utils (
    isfree,
    transformMTE,
    ) where

import Data.Functor

import Smten.Name
import Smten.Sig
import Smten.Type
import Smten.Exp.Exp
import Smten.Exp.Sugar

-- | Check if the variable with given name is free in the given expression.
isfree :: Name -> Exp -> Bool
isfree n = 
  let f :: Exp -> Bool
      f (LitE {}) = False
      f (ConE {}) = False
      f (VarE (Sig nm _)) = n == nm
      f (AppE a b) = f a || f b
      f (LamE (Sig nm _) b) = (n /= nm) && f b
      f (CaseE x _ y n) = any f [x, y, n]
  in f

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

