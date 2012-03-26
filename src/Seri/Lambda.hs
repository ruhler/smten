
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Seri.Lambda (
    ArrowT(..),
    VarE(..), LamE(..), AppE(..)
    ) where

import Seri.Elaborate
import Seri.Name
import Seri.TypeCheck

data VarE t = VarE t Name
    deriving(Show, Eq)

instance (Inject (VarE t) e) => Elaborate (VarE t) e where
    elaborate x = inject x
    reduce n v (VarE _ nm) | n == nm = v
    reduce _ _ v = inject v

instance (Eq t, Show t) => TypeCheck t (VarE t) where
    checkvars n v e@(VarE t vn) =
        if vn == n
            then typeassert v t e
            else return ()
    
data ArrowT t = ArrowT t t
    deriving(Show, Eq)

data LamE t e = LamE t Name e
    deriving(Show, Eq)

instance (Inject (LamE t e) e, Elaborate e e)
        => Elaborate (LamE t e) e where
    elaborate x = inject x
    reduce n v (LamE _ nm b) | n /= nm = reduce n v b
    reduce _ _ l = inject l

instance (TypeCheck t e) => TypeCheck t (LamE t e) where
    checkvars n v e@(LamE _ ln b) =
        if ln /= n
            then checkvars n v b
            else return ()
    
data AppE t e = AppE t e e
    deriving(Show, Eq)

instance (Inject (AppE t e) e, Inject (LamE t e) e, Elaborate e e)
        => Elaborate (AppE t e) e where
    elaborate (AppE t a b) = 
        let a' = elaborate a :: e
            b' = elaborate b :: e
        in case (unject a') of
              Just (LamE t name body :: LamE t e) -> elaborate $ reduce name b body
              Nothing -> inject $ (AppE t a' b' :: AppE t e)

    reduce n v (AppE t a b) =
        let a' = reduce n v a
            b' = reduce n v b
        in inject $ AppE t a' b'
        
instance (TypeCheck t e) => TypeCheck t (AppE t e) where
    checkvars n v (AppE _ a b) = do
        checkvars n v a
        checkvars n v b
    
