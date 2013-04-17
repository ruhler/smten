
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
      f (VarE _ (Sig nm _)) = n == nm
      f (AppE _ a b) = f a || f b
      f (LamE _ (Sig nm _) b) = (n /= nm) && f b
      f (CaseE _ x _ y n) = any f [x, y, n]
      f (LetE _ bs x) = (n `notElem` [nm | (Sig nm _, _) <- bs])
                     && any f (x : map snd bs)
  in f

-- Perform a monadic transformation on all the types appearing in the given
-- expression
transformMTE :: (Functor m, Monad m) => (Type -> m Type) -> Exp -> m Exp
transformMTE f e =
  let me = transformMTE f
  in case e of
       LitE {} -> return e
       ConE l (Sig n t) -> (ConE l . Sig n) <$> f t
       VarE l (Sig n t) -> (VarE l . Sig n) <$> f t
       AppE l a b -> do
            [a', b'] <- mapM me [a, b]
            return $ AppE l a' b'
       LamE l (Sig n t) b -> do
          t' <- f t
          b' <- me b
          return $ LamE l (Sig n t') b'
       CaseE l x (Sig kn kt) y n -> do
         kt' <- f kt
         [x', y', n'] <- mapM me [x, y, n]
         return $ CaseE l x' (Sig kn kt') y' n'
       LetE l bs x -> do
         let g (Sig n t, v) = do
                t' <- f t
                v' <- me v
                return (Sig n t', v')
         bs' <- mapM g bs
         x' <- me x
         return $ LetE l bs' x'

instance Assign Exp where
   assignl f e =
    let me = assignl f 
        mt = assignl f
    in case e of
         LitE {} -> e
         ConE l (Sig n t) -> ConE l (Sig n (mt t))
         VarE l (Sig n t) -> VarE l (Sig n (mt t))
         AppE l a b -> AppE l (me a) (me b)
         LamE l (Sig n t) b -> LamE l (Sig n (mt t)) (me b)
         CaseE l x (Sig kn kt) y n -> CaseE l (me x) (Sig kn (mt kt)) (me y) (me n)
         LetE l bs x -> LetE l [(Sig n (mt t), me v) | (Sig n t, v) <- bs] (me x)

