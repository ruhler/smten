
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.HaskellF.Symbolic (
    Symbolic(..), Symbolic1(..), Symbolic2(..), Symbolic3(..), Symbolic4(..),
    SmtenS(..),
    conS, de_conS, caseS, primS, unaryS, binaryS, 
    ) where

import Smten.Name
import Smten.Type
import Smten.Sig
import Smten.ExpH
import Smten.Prim

-- | Symbolic represents new type wrappers around ExpH.
class (SmtenT a) => Symbolic a where
    box :: ExpH -> a
    unbox :: a -> ExpH

class (SmtenT1 m) => Symbolic1 m where
    box1 :: (Symbolic a) => ExpH -> m a
    unbox1 :: (Symbolic a) => m a -> ExpH 

class (SmtenT2 m) => Symbolic2 m where
    box2 :: (Symbolic a, Symbolic b) => ExpH -> m a b
    unbox2 :: (Symbolic a, Symbolic b) => m a b -> ExpH

class (SmtenT3 m) => Symbolic3 m where
    box3 :: (Symbolic a, Symbolic b, Symbolic c) => ExpH -> m a b c
    unbox3 :: (Symbolic a, Symbolic b, Symbolic c) => m a b c -> ExpH

class (SmtenT4 m) => Symbolic4 m where
    box4 :: (Symbolic a, Symbolic b, Symbolic c, Symbolic d) => ExpH -> m a b c d
    unbox4 :: (Symbolic a, Symbolic b, Symbolic c, Symbolic d) => m a b c d -> ExpH

instance (Symbolic1 m, Symbolic a) => Symbolic (m a) where
    box = box1
    unbox = unbox1

instance (Symbolic2 m, Symbolic a) => Symbolic1 (m a) where
    box1 = box2
    unbox1 = unbox2

instance (Symbolic3 m, Symbolic a) => Symbolic2 (m a) where
    box2 = box3
    unbox2 = unbox3

instance (Symbolic4 m, Symbolic a) => Symbolic3 (m a) where
    box3 = box4
    unbox3 = unbox4


instance Symbolic2 (->) where
    box2 e = \x -> box $ appEH e (unbox x)
    unbox2 f =
       let ta :: (a -> b) -> a
           ta _ = undefined
    
           tb :: (a -> b) -> b
           tb _ = undefined
       in lamEH (Sig (name "x") (smtenT (ta f))) (smtenT (tb f)) $ \x ->
            unbox (f (box x))

-- | Convert a concrete haskell value to its symbolic representation.
class (SmtenEH c, Symbolic f) => SmtenS c f where
    smtenS :: c -> f
    smtenS = box . smtenEH

    de_smtenS :: f -> Maybe c
    de_smtenS = de_smtenEH . unbox

conS :: (Symbolic a) => a -> String -> [ExpH] -> ExpH
conS x nm args = aconEH (name nm) (smtenT x) args

de_conS :: String -> ExpH -> Maybe [ExpH]
de_conS nm = de_kconEH (name nm)

caseS :: (Symbolic x, Symbolic y, Symbolic n)
         => String -> x -> y -> n -> n
caseS k x y n =
  let r = box $ caseEH (unbox x) (Sig (name k) t) (unbox y) (unbox n)
      tys = de_arrowsT $ smtenT y
      tns = de_arrowsT $ smtenT n
      tx = smtenT x
      t = arrowsT (take (length tys - length tns) tys ++ [tx])
  in r

primS :: (Symbolic a) => Prim -> a
primS p = 
 let z = box $ primEH p (smtenT z)
 in z

unaryS :: (SmtenS ca fa, SmtenS cb fb)
            => Prim -> (ca -> cb) -> fa -> fb
unaryS p f a
 | Just av <- de_smtenS a = smtenS (f av)
 | otherwise = primS p a


binaryS :: (SmtenS ca fa, SmtenS cb fb, SmtenS cc fc)
           => Prim -> (ca -> cb -> cc) -> fa -> fb -> fc
binaryS p f a b
 | Just av <- de_smtenS a
 , Just bv <- de_smtenS b = smtenS (f av bv)
 | otherwise = primS p a b

