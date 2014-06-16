
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Implementation of Smten primitive Char# type.
module Smten.Runtime.Char (
    Char#(..), eqChar#, neChar#, gtChar#, geChar#, ltChar#, leChar#,
    primCharApp, toHSChar#, char#, __isLitChar#,
  ) where

import qualified Prelude as P
import qualified GHC.Prim as P

import Smten.Runtime.Bool
import Smten.Runtime.Formula
import Smten.Runtime.Formula.PartialF
import Smten.Runtime.Select
import Smten.Runtime.SmtenHS
import Smten.Runtime.StableNameEq

data Char# =
    Char# P.Char#
  | Ite_Char# BoolF Char# Char#
  | Unreachable_Char#

char# :: P.Char# -> Char#
char# x = Char# x

__isLitChar# :: P.Char# -> Char# -> Bool
__isLitChar# v s = char# v `eqChar#` s

toHSChar# :: Char# -> P.Char#
toHSChar# (Char# x) = x

instance SmtenHS0 Char# where
    ite0 p a b = iteS p a b P.$
        case (select a b, a, b) of
           (SRBoth, Char# av, Char# bv) | av `P.eqChar#` bv -> a
           (SRBoth, _, _) | a `stableNameEq` b -> a
           (SRBoth, Unreachable_Char#, _) -> b
           (SRBoth, _, Unreachable_Char#) -> a
           (SRLeft, Unreachable_Char#, _) -> b
           (SRRight, _, Unreachable_Char#) -> a
           _ -> Ite_Char# p a b

    unreachable0 = Unreachable_Char#

-- TODO: This is a bit of a hack.
-- Can we avoid this? Perhaps by splitting Char into a finite and partial
-- parts like we do for Int?
instance Finite Char# where
    ite_finite p a b = ite0 (finiteF p) a b
    unreachable_finite = Unreachable_Char#

primCharApp :: (SmtenHS0 a) => (P.Char# -> a) -> Char# -> a
primCharApp f x =
  case x of
      Char# c -> f c
      Ite_Char# p a b -> ite0 p (primCharApp f a) (primCharApp f b)
      Unreachable_Char# -> unreachable
  

eqChar# :: Char# -> Char# -> Bool
eqChar# a b = {-# SCC "PRIM_CHAR_EQ" #-} 
   primCharApp (\x ->
   primCharApp (\y ->
       if (P.eqChar# x y) then __True else __False)) a b 

neChar# :: Char# -> Char# -> Bool
neChar# a b = {-# SCC "PRIM_CHAR_NE" #-}
   primCharApp (\x ->
   primCharApp (\y ->
       if (P.neChar# x y) then __True else __False)) a b 

gtChar# :: Char# -> Char# -> Bool
gtChar# a b = {-# SCC "PRIM_CHAR_GT" #-}
   primCharApp (\x ->
   primCharApp (\y ->
       if (P.gtChar# x y) then __True else __False)) a b 

geChar# :: Char# -> Char# -> Bool
geChar# a b = {-# SCC "PRIM_CHAR_GE" #-}
   primCharApp (\x ->
   primCharApp (\y ->
       if (P.geChar# x y) then __True else __False)) a b 

ltChar# :: Char# -> Char# -> Bool
ltChar# a b = {-# SCC "PRIM_CHAR_LT" #-}
   primCharApp (\x ->
   primCharApp (\y ->
       if (P.ltChar# x y) then __True else __False)) a b

leChar# :: Char# -> Char# -> Bool
leChar# a b = {-# SCC "PRIM_CHAR_LE" #-}
   primCharApp (\x ->
   primCharApp (\y ->
       if (P.leChar# x y) then __True else __False)) a b

