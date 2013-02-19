
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.HaskellF.HaskellF (
    HaskellF(..), HaskellF1(..), HaskellF2(..), HaskellF3(..), HaskellF4(..),
    SmtenHF(..),
    conHF, de_conHF, caseHF, primHF, unaryHF, binaryHF, 
    ) where

import Smten.Name
import Smten.Type
import Smten.Sig
import Smten.ExpH
import Smten.Prim

-- | HaskellF represents haskell data types with extra ExpH leg.
class (SmtenT a) => HaskellF a where
    box :: ExpH -> a
    unbox :: a -> ExpH

class (SmtenT1 m) => HaskellF1 m where
    box1 :: (HaskellF a) => ExpH -> m a
    unbox1 :: (HaskellF a) => m a -> ExpH 

class (SmtenT2 m) => HaskellF2 m where
    box2 :: (HaskellF a, HaskellF b) => ExpH -> m a b
    unbox2 :: (HaskellF a, HaskellF b) => m a b -> ExpH

class (SmtenT3 m) => HaskellF3 m where
    box3 :: (HaskellF a, HaskellF b, HaskellF c) => ExpH -> m a b c
    unbox3 :: (HaskellF a, HaskellF b, HaskellF c) => m a b c -> ExpH

class (SmtenT4 m) => HaskellF4 m where
    box4 :: (HaskellF a, HaskellF b, HaskellF c, HaskellF d) => ExpH -> m a b c d
    unbox4 :: (HaskellF a, HaskellF b, HaskellF c, HaskellF d) => m a b c d -> ExpH

instance (HaskellF1 m, HaskellF a) => HaskellF (m a) where
    box = box1
    unbox = unbox1

instance (HaskellF2 m, HaskellF a) => HaskellF1 (m a) where
    box1 = box2
    unbox1 = unbox2

instance (HaskellF3 m, HaskellF a) => HaskellF2 (m a) where
    box2 = box3
    unbox2 = unbox3

instance (HaskellF4 m, HaskellF a) => HaskellF3 (m a) where
    box3 = box4
    unbox3 = unbox4


instance HaskellF2 (->) where
    box2 e = \x -> box $ appEH e (unbox x)
    unbox2 f =
       let ta :: (a -> b) -> a
           ta _ = undefined
    
           tb :: (a -> b) -> b
           tb _ = undefined
       in lamEH (Sig (name "x") (smtenT (ta f))) (smtenT (tb f)) $ \x ->
            unbox (f (box x))

-- | Convert a concrete haskell value to its HaskellF representation.
class (SmtenEH c, HaskellF f) => SmtenHF c f where
    smtenHF :: c -> f
    smtenHF = box . smtenEH

    de_smtenHF :: f -> Maybe c
    de_smtenHF = de_smtenEH . unbox

conHF :: (HaskellF a) => a -> String -> [ExpH] -> ExpH
conHF x nm args = aconEH (name nm) (smtenT x) args

de_conHF :: String -> ExpH -> Maybe [ExpH]
de_conHF nm = de_kconEH (name nm)

caseHF :: (HaskellF x, HaskellF y, HaskellF n)
         => String -> x -> y -> n -> n
caseHF k x y n =
  let r = box $ caseEH (unbox x) (Sig (name k) t) (unbox y) (unbox n)
      tys = de_arrowsT $ smtenT y
      tns = de_arrowsT $ smtenT n
      tx = smtenT x
      t = arrowsT (take (length tys - length tns) tys ++ [tx])
  in r

primHF :: (HaskellF a) => Prim -> a
primHF p = 
 let z = box $ primEH p (smtenT z)
 in z

unaryHF :: (SmtenHF ca fa, SmtenHF cb fb)
            => Prim -> (ca -> cb) -> fa -> fb
unaryHF p f a
 | Just av <- de_smtenHF a = smtenHF (f av)
 | otherwise = primHF p a


binaryHF :: (SmtenHF ca fa, SmtenHF cb fb, SmtenHF cc fc)
           => Prim -> (ca -> cb -> cc) -> fa -> fb -> fc
binaryHF p f a b
 | Just av <- de_smtenHF a
 , Just bv <- de_smtenHF b = smtenHF (f av bv)
 | otherwise = primHF p a b

