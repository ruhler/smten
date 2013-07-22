
{-# LANGUAGE FlexibleInstances #-}

module Smten.Runtime.SmtenHS (
    SmtenHS0(..), SmtenHS1(..), SmtenHS2(..), SmtenHS3(..), SmtenHS4(..),
    ite, realize,
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

class SmtenHS2 m where
    error2 :: (SmtenHS0 a, SmtenHS0 b) => ErrorString -> m a b
    ite2 :: (SmtenHS0 a, SmtenHS0 b) => BoolF -> m a b -> m a b -> m a b
    realize2 :: (SmtenHS0 a, SmtenHS0 b) => Model -> m a b -> m a b

instance (SmtenHS0 a, SmtenHS2 m) => SmtenHS1 (m a) where
    error1 = error2
    ite1 = ite2
    realize1 = realize2

class SmtenHS3 m where
    error3 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c) => ErrorString -> m a b c
    ite3 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c) => BoolF -> m a b c -> m a b c -> m a b c
    realize3 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c) => Model -> m a b c -> m a b c

instance (SmtenHS0 a, SmtenHS3 m) => SmtenHS2 (m a) where
    error2 = error3
    ite2 = ite3
    realize2 = realize3

class SmtenHS4 m where
    error4 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c, SmtenHS0 d) => ErrorString -> m a b c d
    ite4 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c, SmtenHS0 d) => BoolF -> m a b c d -> m a b c d -> m a b c d
    realize4 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c, SmtenHS0 d) => Model -> m a b c d -> m a b c d

instance (SmtenHS0 a, SmtenHS4 m) => SmtenHS3 (m a) where
    error3 = error4
    ite3 = ite4
    realize3 = realize4

ite :: (SmtenHS0 a) => BoolF -> a -> a -> a
ite TrueF a _ = a
ite FalseF _ b = b
ite (NotF x) a b = ite x b a
ite p a b = ite0 p a b
    
realize :: (SmtenHS0 a) => Model -> a -> a
realize m x = m_cached m realize0 x

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
