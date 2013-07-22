
{-# LANGUAGE FlexibleInstances #-}

module Smten.Runtime.SmtenHS (
    SmtenHS0(..), SmtenHS1(..), ite, realize,
    ) where

import Smten.Runtime.ErrorString
import Smten.Runtime.Formula
import Smten.Runtime.Model

class SmtenHS0 a where
    error0 :: ErrorString -> a
    ite0 :: BoolF -> a -> a -> a
    realize0 :: Model -> a -> a

class SmtenHS1 m where
    error1 :: (SmtenHS0 a) => ErrorString -> m a
    ite1 :: (SmtenHS0 a) => BoolF -> m a -> m a -> m a
    realize1 :: (SmtenHS0 a) => Model -> m a -> m a

instance (SmtenHS0 a, SmtenHS1 m) => SmtenHS0 (m a) where
    error0 = error1
    ite0 = ite1
    realize0 = realize1

ite :: (SmtenHS0 a) => BoolF -> a -> a -> a
ite TrueF a _ = a
ite FalseF _ b = b
ite (NotF x) a b = ite x b a
ite p a b = ite0 p a b
    
realize :: (SmtenHS0 a) => Model -> a -> a
realize = error "TODO: realize"

instance SmtenHS0 BoolF where
    error0 = error "TODO: BoolF.error0"
    ite0 = IteF
    realize0 m x =
      case x of
        TrueF -> TrueF
        FalseF -> FalseF
        IteF p a b -> ite (realize m p) (realize m a) (realize m b)
        AndF a b -> andF (realize m a) (realize m b)
        NotF p -> notF (realize m p)
        VarF n -> lookupBoolF m n
