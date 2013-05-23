
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.HaskellF.HaskellF (
    HaskellF(..), HaskellF1(..), HaskellF2(..), HaskellF3(..), HaskellF4(..),
    SmtenHF(..),
    Function(..), lamHF, applyHF,
    conHF, de_conHF, caseHF, primHF, unaryHF,
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

newtype Function a b = Function {
    function_unbox :: ExpH
}

instance SmtenT2 Function where
    smtenT2 x = conT arrowN

instance HaskellF2 Function where
    box2 = Function
    unbox2 = function_unbox

applyHF :: (HaskellF a, HaskellF b) => Function a b -> a -> b
applyHF f x = box $ appEH (unbox f) (unbox x)

lamHF :: (HaskellF a, HaskellF b) => String -> (a -> b) -> Function a b
lamHF n f =
  let g :: ExpH -> ExpH
      g x = unbox $ f (box x)

      r = box $ lamEH (Sig (name n) ta) tb g
      Just (ta, tb) = de_arrowT (smtenT r)
  in r

-- | Convert a concrete haskell value to its HaskellF representation.
class (SmtenEH c, HaskellF f) => SmtenHF c f where
    smtenHF :: c -> f
    smtenHF = box . smtenEH

    de_smtenHF :: f -> Maybe c
    de_smtenHF = de_smtenEH . unbox

instance (SmtenEH a, HaskellF a) => SmtenHF a a where
    smtenHF = id
    de_smtenHF = return

conHF :: (HaskellF a) => a -> String -> [ExpH] -> ExpH
conHF x nm args = aconEH (name nm) (smtenT x) args

de_conHF :: String -> ExpH -> Maybe [ExpH]
de_conHF nm = de_kconEH (name nm)

caseHF :: (HaskellF x, HaskellF y, HaskellF n)
         => String -> x -> y -> n -> n
caseHF k x y n =
  let tys = de_arrowsT $ smtenT y
      tcs = smtenT n
      tns = de_arrowsT tcs
      r = box $ caseEH tcs (unbox x) (Sig (name k) t) (unbox y) (unbox n)
      tx = smtenT x
      t = arrowsT (take (length tys - length tns) tys ++ [tx])
  in r

primHF :: (HaskellF a) => Prim -> a
primHF p = 
 let z = box $ primEH p (smtenT z)
 in z

unaryHF :: (SmtenHF ca fa, SmtenHF cb fb) => PrimF (ca -> cb) -> Function fa fb
unaryHF p = primHF (p_prim p)

