
{-# LANGUAGE FlexibleInstances #-}

module Seri.HaskellF.Symbolic (
    Symbolic(..), Symbolic1(..), Symbolic2(..), Symbolic3(..), Symbolic4(..),
    seriS, de_seriS,
    conS, caseS, primS,
    nullaryS, unaryS, binaryS, 
    ) where

import Seri.Name
import Seri.Type
import Seri.Sig
import Seri.ExpH
import Seri.Prim

-- | Symbolic represents new type wrappers around ExpH.
class (SeriT a) => Symbolic a where
    box :: ExpH -> a
    unbox :: a -> ExpH

class (SeriT1 m) => Symbolic1 m where
    box1 :: (Symbolic a) => ExpH -> m a
    unbox1 :: (Symbolic a) => m a -> ExpH 

class (SeriT2 m) => Symbolic2 m where
    box2 :: (Symbolic a, Symbolic b) => ExpH -> m a b
    unbox2 :: (Symbolic a, Symbolic b) => m a b -> ExpH

class (SeriT3 m) => Symbolic3 m where
    box3 :: (Symbolic a, Symbolic b, Symbolic c) => ExpH -> m a b c
    unbox3 :: (Symbolic a, Symbolic b, Symbolic c) => m a b c -> ExpH

class (SeriT4 m) => Symbolic4 m where
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
       in lamEH (Sig (name "x") (seriT (ta f))) (seriT (tb f)) $ \x ->
            unbox (f (box x))

-- | Convert a concrete haskell value to its symbolic representation.
seriS :: (SeriEH c, Symbolic f) => c -> f
seriS = box . seriEH

de_seriS :: (Symbolic f, SeriEH c) => f -> Maybe c
de_seriS = de_seriEH . unbox

conS :: (Symbolic a) => String -> a
conS nm =
  let x = box $ conEH (Sig (name nm) (seriT x))
  in x

caseS :: (Symbolic x, Symbolic y, Symbolic n)
         => String -> x -> y -> n -> n
caseS k x y n =
  let r = box $ caseEH (unbox x) (Sig (name k) t) (unbox y) (unbox n)
      tys = de_arrowsT $ seriT y
      tns = de_arrowsT $ seriT n
      tx = seriT x
      t = arrowsT (take (length tys - length tns) tys ++ [tx])
  in r

primS :: (Symbolic a) => Prim -> a
primS p = 
 let z = box $ primEH p (seriT z)
 in z

nullaryS :: (Symbolic a) => ExpH -> a
nullaryS = box

unaryS :: (Symbolic a, Symbolic b) => (ExpH -> ExpH) -> a -> b
unaryS f x = box $ f (unbox x)

binaryS :: (Symbolic a, Symbolic b, Symbolic c)
           => (ExpH -> ExpH -> ExpH) -> a -> b -> c
binaryS f a b = box $ f (unbox a) (unbox b)

