
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fprof-auto-top #-}

module Smten.Runtime.SmtenHS (
    SmtenHS0(..), SmtenHS1(..), SmtenHS2(..), SmtenHS3(..), SmtenHS4(..),
    ite, unreachable,
    emptycase,
    ) where

import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula

class SmtenHS0 a where
    -- It can be assumed that the given BoolF is not concrete.
    ite0 :: BoolF -> a -> a -> a
    unreachable0 :: a

class SmtenHS1 m where
    ite1 :: (SmtenHS0 a) => BoolF -> m a -> m a -> m a
    unreachable1 :: (SmtenHS0 a) => m a

instance (SmtenHS0 a, SmtenHS1 m) => SmtenHS0 (m a) where
    ite0 = ite1
    unreachable0 = unreachable1

class SmtenHS2 m where
    ite2 :: (SmtenHS0 a, SmtenHS0 b) => BoolF -> m a b -> m a b -> m a b
    unreachable2 :: (SmtenHS0 a, SmtenHS0 b) => m a b

instance (SmtenHS0 a, SmtenHS2 m) => SmtenHS1 (m a) where
    ite1 = ite2
    unreachable1 = unreachable2

class SmtenHS3 m where
    ite3 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c) => BoolF -> m a b c -> m a b c -> m a b c
    unreachable3 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c) => m a b c

instance (SmtenHS0 a, SmtenHS3 m) => SmtenHS2 (m a) where
    ite2 = ite3
    unreachable2 = unreachable3

class SmtenHS4 m where
    ite4 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c, SmtenHS0 d) => BoolF -> m a b c d -> m a b c d -> m a b c d
    unreachable4 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c, SmtenHS0 d) =>  m a b c d


instance (SmtenHS0 a, SmtenHS4 m) => SmtenHS3 (m a) where
    ite3 = ite4
    unreachable3 = unreachable4

{-# INLINEABLE ite #-}
ite :: (SmtenHS0 a) => BoolF -> a -> a -> a
ite p a b 
  | isTrueF p = a
  | isFalseF p = b
  | otherwise = ite0 p a b

{-# INLINEABLE unreachable #-}
unreachable :: (SmtenHS0 a) => a
unreachable = unreachable0

instance SmtenHS0 BoolFF where
    ite0 = error "BoolFF.ite"
    unreachable0 = Unreachable_BoolFF

instance SmtenHS0 IntegerFF where
    ite0 = error "IntegerFF.ite"
    unreachable0 = Unreachable_IntegerFF

instance SmtenHS0 BitFF where
    ite0 = error "BitFF.ite"
    unreachable0 = Unreachable_BitFF
              
instance SmtenHS0 BoolF where
    ite0 = iteF
    unreachable0 = unreachableF

instance SmtenHS0 IntegerF where
    ite0 = ite_IntegerF
    unreachable0 = unreachable_IntegerF
      
   
instance SmtenHS0 (BitF n) where
    ite0 = ite_BitF
    unreachable0 = unreachable_BitF

instance SmtenHS2 (->) where
    ite2 p fa fb = {-# SCC "ITE2_Fun" #-} \x -> ite0 p (fa x) (fb x)
    unreachable2 = \x -> unreachable

emptycase :: a
emptycase = error "inaccessable case"

