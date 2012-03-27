
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Seri.Arithmetic (
    IntegerT(..), IntegerE(..),
    AddE(..), MulE(..)) 
    where

import Seri.Elaborate
import Seri.Ppr
import Seri.TypeCheck

data IntegerT = IntegerT
    deriving(Show, Eq)

instance Ppr IntegerT where
    ppr _ = text "Integer"

data IntegerE = IntegerE Integer
    deriving(Show, Eq)

instance Ppr IntegerE where
    ppr (IntegerE i) = integer i

instance (Inject IntegerE e)
    => Elaborate IntegerE e where
    elaborate x = inject x
    reduce _ _ x = inject x

instance (Eq t, Show t, Inject IntegerT t) => TypeCheck t IntegerE where
    typeof _ = inject $ IntegerT
    typecheck _ = return ()
    checkvars _ _ _ = return ()

data AddE e = AddE e e
    deriving(Show, Eq)

instance (Ppr e) => Ppr (AddE e) where
    ppr (AddE a b) = parens (ppr a) <+> text "+" <+> parens (ppr b)

instance (Elaborate e e, Inject (AddE e) e, Inject IntegerE e)
    => Elaborate (AddE e) e where
    elaborate (AddE a b) =
        let a' = elaborate a :: e
            b' = elaborate b :: e
        in case (unject a', unject b') of
              (Just (IntegerE av), Just (IntegerE bv)) -> inject (IntegerE $ av+bv)
              _ -> inject $ (AddE a' b' :: AddE e)

    reduce n v (AddE a b) =
        let a' = reduce n v a
            b' = reduce n v b
        in inject $ (AddE a' b' :: AddE e)

instance
    (Eq t, Ppr t, Ppr e, TypeCheck t e, Inject IntegerT t)
    => TypeCheck t (AddE e) where

    typeof _ = inject IntegerT

    typecheck (AddE a b) = do
        typecheck a
        typecheck b
        typeassert (inject IntegerT) (typeof a) a
        typeassert (inject IntegerT) (typeof b) b

    checkvars n v (AddE a b) = do
        checkvars n v a
        checkvars n v b
        
                
data MulE e = MulE e e
    deriving(Show, Eq)

instance (Ppr e) => Ppr (MulE e) where
    ppr (MulE a b) = parens (ppr a) <+> text "*" <+> parens (ppr b)

instance (Elaborate e e, Inject (MulE e) e, Inject IntegerE e)
    => Elaborate (MulE e) e where
    elaborate (MulE a b) =
        let a' = elaborate a :: e
            b' = elaborate b :: e
        in case (unject a', unject b') of
              (Just (IntegerE av), Just (IntegerE bv)) -> inject (IntegerE $ av*bv)
              _ -> inject $ (MulE a' b' :: MulE e)

    reduce n v (MulE a b) =
        let a' = reduce n v a
            b' = reduce n v b
        in inject $ (MulE a' b' :: MulE e)
                
instance
    (Eq t, Ppr t, Ppr e, TypeCheck t e, Inject IntegerT t)
    => TypeCheck t (MulE e) where

    typeof _ = inject IntegerT

    typecheck (MulE a b) = do
        typecheck a
        typecheck b
        typeassert (inject IntegerT) (typeof a) a
        typeassert (inject IntegerT) (typeof b) b

    checkvars n v (MulE a b) = do
        checkvars n v a
        checkvars n v b
