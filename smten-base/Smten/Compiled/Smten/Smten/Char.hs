
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Char (
    Char(..),
    toHSChar,
  ) where

import qualified Prelude as P
import qualified GHC.Types as P
import qualified GHC.Prim as P

import Smten.Runtime.Formula
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf

data Char =
    C# P.Char#
  | Ite_Char BoolF Char Char
  | Unreachable_Char

instance SymbolicOf P.Char Char where
    tosym (P.C# x) = C# x

    symapp f x =
      case x of
        C# c -> f (P.C# c)
        Ite_Char p a b -> ite0 p (f $$ a) (f $$ b)
        Unreachable_Char -> unreachable

toHSChar :: Char -> P.Char
toHSChar (C# x) = P.C# x

instance SmtenHS0 Char where
    ite0 = Ite_Char
    unreachable0 = Unreachable_Char

