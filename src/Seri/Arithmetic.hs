
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Seri.Arithmetic (IntegerE(..), AddE(..)) 
    where

import Seri.Elaborate

data IntegerE = IntegerE Integer
    deriving(Show, Eq)

data AddE e where
    AddE :: (Elaborate a e, Elaborate b e) => a -> b -> AddE e

instance Show (AddE e) where
    show (AddE a b) = "AddE " ++ show a ++ " " ++ show b

instance Eq (AddE e) where
    (==) (AddE a b) (AddE c d) = False
    
instance (Show e, Inject IntegerE e)
    => Elaborate IntegerE e where
    elaborate x = inject x

instance (Elaborate e e, Show e, Inject (AddE e) e, Inject IntegerE e)
    => Elaborate (AddE e) e where
    elaborate (AddE a b) =
        let a' = elaborate a :: e
            b' = elaborate b :: e
        in case (unject a', unject b') of
              (Just (IntegerE av), Just (IntegerE bv)) -> inject (IntegerE $ av+bv)
              _ -> inject $ (AddE a' b' :: AddE e)
                
