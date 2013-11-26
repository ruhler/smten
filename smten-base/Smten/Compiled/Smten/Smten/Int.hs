
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Int (
    Int(..),
  ) where

import qualified Prelude as P
import qualified GHC.Types as P
import qualified GHC.Prim as P

import Smten.Runtime.Formula
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf

data Int =
    I# P.Int#
  | Int_Ite Bool Int Int

instance SymbolicOf P.Int Int where
    tosym (P.I# x) = I# x

    {-# INLINEABLE symapp #-}
    {-# SPECIALIZE symapp :: (P.Int -> Bool) -> Int -> Bool #-}
    symapp f x =
      case x of
        I# i -> f (P.I# i)
        Int_Ite p a b -> ite0 p (f $$ a) (f $$ b)

instance SmtenHS0 Int where
    realize0 m x = 
      case x of
        I# {} -> x
        Int_Ite p a b -> iterealize p a b m
    ite0 = Int_Ite

instance P.Num Int where
    fromInteger = tosym P.. (P.fromInteger :: P.Integer -> P.Int)
    (+) = P.error "Smten Int P.Num (+) not supported"
    (*) = P.error "Smten Int P.Num (*) not supported"
    abs = P.error "Smten Int P.Num abs not supported"
    signum = P.error "Smten Int P.Num signum not supported"
