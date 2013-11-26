
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Smten.Runtime.SmtenHS (
    SmtenHS0(..), SmtenHS1(..), SmtenHS2(..), SmtenHS3(..), SmtenHS4(..),
    ite, iterealize, realize,
    merge, emptycase, unusedfield,
    ) where

import qualified Prelude as P
import Prelude hiding (Bool(..), Integer)

import Smten.Runtime.Model
import Smten.Runtime.StableNameEq
import Smten.Runtime.Formula

class SmtenHS0 a where
    ite0 :: Bool -> a -> a -> a
    realize0 :: Model -> a -> a

class SmtenHS1 m where
    ite1 :: (SmtenHS0 a) => Bool -> m a -> m a -> m a
    realize1 :: (SmtenHS0 a) => Model -> m a -> m a

instance (SmtenHS0 a, SmtenHS1 m) => SmtenHS0 (m a) where
    ite0 = ite1
    realize0 = realize1

class SmtenHS2 m where
    ite2 :: (SmtenHS0 a, SmtenHS0 b) => Bool -> m a b -> m a b -> m a b
    realize2 :: (SmtenHS0 a, SmtenHS0 b) => Model -> m a b -> m a b

instance (SmtenHS0 a, SmtenHS2 m) => SmtenHS1 (m a) where
    ite1 = ite2
    realize1 = realize2

class SmtenHS3 m where
    ite3 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c) => Bool -> m a b c -> m a b c -> m a b c
    realize3 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c) => Model -> m a b c -> m a b c

instance (SmtenHS0 a, SmtenHS3 m) => SmtenHS2 (m a) where
    ite2 = ite3
    realize2 = realize3

class SmtenHS4 m where
    ite4 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c, SmtenHS0 d) => Bool -> m a b c d -> m a b c d -> m a b c d
    realize4 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c, SmtenHS0 d) => Model -> m a b c d -> m a b c d


instance (SmtenHS0 a, SmtenHS4 m) => SmtenHS3 (m a) where
    ite3 = ite4
    realize3 = realize4

{-# INLINEABLE ite #-}
ite :: (SmtenHS0 a) => Bool -> a -> a -> a
ite True a _ = a
ite False _ b = b
ite (Bool_Not x) a b = ite x b a
ite p a b 
  | a `stableNameEq` b = a
  | otherwise = ite0 p a b

{-# INLINEABLE iterealize #-}
iterealize :: (SmtenHS0 a) => Bool -> a -> a -> Model -> a
iterealize p a b m = ite (realize m p) (realize m a) (realize m b)
    
realize :: (SmtenHS0 a) => Model -> a -> a
realize m x = m_cached m realize0 x

-- merge
-- Merge all the arguments into a single object.
-- Assumptions: It is always that case that at least one of the guards is True.
--  Which also means: this must be given a non-empty list.
merge :: (SmtenHS0 a) => [(Bool, a)] -> a
merge xs =
  let notfalse False = P.False
      notfalse _ = P.True
      xs' = filter (notfalse . fst) xs

      f [] = error "merge on empty list"
      f [(_, v)] = v
      f ((p, v):vs) = ite p v (f vs)
  in f xs'

instance SmtenHS0 Bool where
    ite0 = iteF
    realize0 m x =
      case x of
        True -> True
        False -> False
        Bool_Ite p a b -> iterealize p a b m
        Bool_And a b -> andF (realize m a) (realize m b)
        Bool_Not p -> notF (realize m p)
        Bool_EqInteger a b -> eq_Integer (realize m a) (realize m b)
        Bool_LeqInteger a b -> leq_Integer (realize m a) (realize m b)
        Bool_EqBit w a b -> eq_Bit w (realize m a) (realize m b)
        Bool_LeqBit w a b -> leq_Bit w (realize m a) (realize m b)
        Bool_Var n -> lookupBool m n

instance SmtenHS0 Integer where
   ite0 = Integer_Ite
   realize0 m x = 
      case x of
         Integer {} -> x
         Integer_Add a b -> add_Integer (realize m a) (realize m b)
         Integer_Sub a b -> sub_Integer (realize m a) (realize m b)
         Integer_Ite p a b -> iterealize p a b m
         Integer_Var n -> lookupInteger m n
   
instance SmtenHS0 (Bit n) where
    realize0 m x = 
      case x of
        Bit {} -> x
        Bit_Add a b -> add_Bit (realize m a) (realize m b)
        Bit_Sub a b -> sub_Bit (realize m a) (realize m b)
        Bit_Mul a b -> mul_Bit (realize m a) (realize m b)
        Bit_Or a b -> or_Bit (realize m a) (realize m b)
        Bit_And a b -> and_Bit (realize m a) (realize m b)
        Bit_Shl a b -> shl_Bit (realize m a) (realize m b)
        Bit_Lshr a b -> lshr_Bit (realize m a) (realize m b)
        Bit_Concat w a b -> concat_Bit w (realize m a) (realize m b)
        Bit_Not a -> not_Bit (realize m a)
        Bit_SignExtend by x -> sign_extend_Bit by (realize m x)
        Bit_Extract wx hi lo x -> extract_Bit wx hi lo (realize m x)
        Bit_Ite p a b -> iterealize p a b m
        Bit_Var w n -> lookupBit m w n
    ite0 = Bit_Ite

instance SmtenHS2 (->) where
    realize2 m f = \x -> realize m (f (realize m x))
    ite2 p fa fb = \x -> ite p (fa x) (fb x)

emptycase :: a
emptycase = error "inaccessable case"

unusedfield :: P.String -> a
unusedfield msg = error ("unused field access " ++ msg)

