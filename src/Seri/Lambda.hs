
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Seri.Lambda (
    ArrowT(..),
    VarE(..), LamE(..), AppE(..)
    ) where

import Seri.Elaborate
import Seri.Name
import Seri.Ppr
import Seri.TypeCheck

data VarE t = VarE t Name
    deriving(Show, Eq)

instance (Ppr (VarE t)) where
    ppr (VarE _ nm) = text nm

instance (Inject (VarE t) e) => Elaborate (VarE t) e where
    elaborate x = inject x
    reduce n v (VarE _ nm) | n == nm = v
    reduce _ _ v = inject v

instance (Eq t, Ppr t) => TypeCheck t (VarE t) where
    typeof (VarE t _) = t

    typecheck (VarE _ _) = return ()

    checkvars n v e@(VarE t vn) =
        if vn == n
            then typeassert v t e
            else return ()
    
data ArrowT t = ArrowT t t
    deriving(Show, Eq)

instance (Ppr t) => Ppr (ArrowT t) where
    ppr (ArrowT a b) = parens (ppr a) <+> text "->" <+> parens (ppr b)

data LamE t e = LamE t Name e
    deriving(Show, Eq)

instance (Ppr e) => Ppr (LamE t e) where
    ppr (LamE _ n b) = text "\\" <> text n <+> text "->" <+> ppr b

instance (Inject (LamE t e) e, Elaborate e e)
        => Elaborate (LamE t e) e where
    elaborate x = inject x
    reduce n v (LamE _ nm b) | n /= nm = reduce n v b
    reduce _ _ l = inject l

instance
    (Ppr t, Ppr e, TypeCheck t e, Inject (ArrowT t) t, Eq t)
    => TypeCheck t (LamE t e) where

    typeof (LamE t _ _) = t

    typecheck e@(LamE t n body) =
        case (unject t) of
            Just (ArrowT at bt) -> do
                typecheck body
                checkvars n at body
                typeassert bt (typeof body) body
            Nothing -> typefail "function" t e

    checkvars n v (LamE _ ln b) =
        if ln /= n
            then checkvars n v b
            else return ()
    
data AppE t e = AppE t e e
    deriving(Show, Eq)

instance (Ppr e) => Ppr (AppE t e) where
    ppr (AppE _ f x) = parens (ppr f) <+> parens (ppr x)

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
        
instance
    (Eq t, Ppr t, Ppr e, TypeCheck t e, Inject (ArrowT t) t)
    => TypeCheck t (AppE t e) where

    typeof (AppE t _ _) = t

    typecheck e@(AppE t f x) = do
        typecheck f
        typecheck x
        case (unject (typeof f)) of
            Just (ArrowT at bt) -> do
                typeassert at (typeof x) x
                typeassert bt t e
            Nothing -> typefail "function" (typeof f) f

    checkvars n v (AppE _ a b) = do
        checkvars n v a
        checkvars n v b
    
