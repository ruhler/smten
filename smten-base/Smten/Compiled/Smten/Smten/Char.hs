
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Char (
    Char(..), __C#, __applyToChar,
    toHSChar,
  ) where

import qualified Prelude as P
import qualified GHC.Types as P
import qualified GHC.Prim as P

import Smten.Runtime.Formula
import Smten.Runtime.Select
import Smten.Runtime.SmtenHS
import Smten.Runtime.StableNameEq
import Smten.Runtime.SymbolicOf

data Char =
    C# P.Char#
  | Ite_Char BoolF Char Char
  | Unreachable_Char

__C# = C#

instance SymbolicOf P.Char Char where
    tosym (P.C# x) = C# x
    symapp f x = __applyToChar (\v -> f (P.C# v)) x

__applyToChar :: (SmtenHS0 a) => (P.Char# -> a) -> Char -> a
__applyToChar f x =
  case x of
    C# c -> f c
    Ite_Char p a b -> ite0 p (__applyToChar f a) (__applyToChar f b)
    Unreachable_Char -> unreachable

toHSChar :: Char -> P.Char
toHSChar (C# x) = P.C# x

instance SmtenHS0 Char where
    ite0 p a b = 
        case (select a b, a, b) of
           (SRBoth, C# av, C# bv) | P.isTrue# (av `P.eqChar#` bv) -> a
           (SRBoth, _, _) | a `stableNameEq` b -> a
           (SRBoth, Unreachable_Char, _) -> b
           (SRBoth, _, Unreachable_Char) -> a
           (SRLeft, Unreachable_Char, _) -> b
           (SRRight, _, Unreachable_Char) -> a
           _ -> Ite_Char p a b
    unreachable0 = Unreachable_Char

