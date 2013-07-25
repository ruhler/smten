

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Integer (
    Integer(..),
  ) where

import qualified Prelude as P

import Smten.Runtime.ErrorString
import Smten.Runtime.Formula
import Smten.Runtime.Model
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf

data Integer =
    Integer P.Integer
  | Integer_Ite BoolF Integer Integer
  | Integer_Err ErrorString
  | Integer_Prim (Model -> Integer) Integer

instance SymbolicOf P.Integer Integer where
    tosym = Integer

    symapp f x =
      case x of
        Integer i -> f i
        Integer_Ite p a b -> ite0 p (f $$ a) (f $$ b)
        Integer_Err msg -> error0 msg
        Integer_Prim r x -> primitive0 (\m -> realize m (f $$ (r m))) (f $$ x)

instance SmtenHS0 Integer where
    error0 = Integer_Err
    realize0 m x =
      case x of
        Integer {} -> x
        Integer_Ite p a b -> iterealize p a b m
        Integer_Err msg -> Integer_Err (realize m msg)
        Integer_Prim r _ -> r m
    ite0 = Integer_Ite
    primitive0 = Integer_Prim

instance P.Num Integer where
    fromInteger = Integer
    (+) = P.error "Smten Integer P.Num (+) not supported"
    (*) = P.error "Smten Integer P.Num (*) not supported"
    abs = P.error "Smten Integer P.Num abs not supported"
    signum = P.error "Smten Integer P.Num signum not supported"

