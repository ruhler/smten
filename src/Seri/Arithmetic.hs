
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Seri.Arithmetic (
    IntegerT(..), IntegerE(..),
    AddE(..), MulE(..)) 
    where

import Seri.Elaborate

data IntegerT = IntegerT
    deriving(Show, Eq)

data IntegerE = IntegerE Integer
    deriving(Show, Eq)

instance (Inject IntegerE e)
    => Elaborate IntegerE e where
    elaborate x = inject x
    reduce _ _ x = inject x

data AddE e = AddE e e
    deriving(Show, Eq)

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
        
                
data MulE e = MulE e e
    deriving(Show, Eq)

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
                
