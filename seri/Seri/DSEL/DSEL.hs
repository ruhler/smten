
{-# LANGUAGE FlexibleInstances #-}

module Seri.DSEL.DSEL ( 
    ExpT(..),
    apply, apply2,
    varET, varET2,
    (Seri.DSEL.DSEL.<), (Seri.DSEL.DSEL.>),
  ) where

import Seri.Name
import Seri.Sig
import Seri.Type.SeriT
import Seri.ExpH.ExpH
import Seri.ExpH.SeriEH
import Seri.ExpH.Sugar

data ExpT a = ExpT ExpH

varET :: (SeriT a) => String -> ExpT a
varET nm =
  let t :: ExpT a -> a
      t _ = undefined
    
      me = ExpT $ varEH (Sig (name nm) (seriT (t me)))
  in me

-- | Make a binary function from a variable name.
varET2 :: (SeriT a, SeriT b, SeriT c)
         => String -> ExpT a -> ExpT b -> ExpT c
varET2 nm =
  let f :: (SeriT a, SeriT b, SeriT c) => ExpT (a -> b -> c)
      f = varET nm
  in apply2 f

apply :: ExpT (a -> b) -> ExpT a -> ExpT b
apply (ExpT f) (ExpT x) = ExpT $ appEH f x

apply2 :: ExpT (a -> b -> c) -> ExpT a -> ExpT b -> ExpT c
apply2 (ExpT f) (ExpT a) (ExpT b) = ExpT $ appsEH f [a, b]


(<) :: ExpT Integer -> ExpT Integer -> ExpT Bool
(<) = varET2 "Prelude.<"

(>) :: ExpT Integer -> ExpT Integer -> ExpT Bool
(>) = varET2 "Prelude.>"

instance Num (ExpT Integer) where
    fromInteger = ExpT . seriEH 
    (+) = varET2 "Prelude.+"
    (*) = varET2 "Prelude.*"
    (-) = varET2 "Prelude.-"
    abs = error $ "todo: abs for TExp Integer"
    signum = error $ "todo: signum for TExp Integer"

