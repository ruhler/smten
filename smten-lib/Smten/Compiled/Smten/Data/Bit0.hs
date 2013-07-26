
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Smten.Compiled.Smten.Data.Bit0 (
    Bit, bv_eq, bv_show, bv_fromInteger, bv_add, bv_sub,
    ) where

import qualified Prelude as P
import GHC.TypeLits
import qualified Smten.Runtime.Bit as P
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf
import Smten.Runtime.Types
import Smten.Compiled.Smten.Smten.Base

data Bit (n :: Nat) =
    Bit P.Bit
  | Bit_Ite Bool (Bit n) (Bit n)
  | Bit_Err ErrorString
  | Bit_Prim (Model -> Bit n) (Bit n)

instance SymbolicOf P.Bit (Bit n) where
    tosym = Bit

    symapp f x = 
      case x of
        Bit b -> f b
        Bit_Ite p a b -> ite0 p (f $$ a) (f $$ b)
        Bit_Err msg -> error0 msg
        Bit_Prim r x -> primitive0 (\m -> realize m (f $$ (r m))) (f $$ x)
   
instance SmtenHS0 (Bit n) where
    error0 = Bit_Err
    realize0 m x = 
      case x of
        Bit {} -> x
        Bit_Ite p a b -> iterealize p a b m
        Bit_Err msg -> Bit_Err (realize m msg)
        Bit_Prim r _ -> r m
    ite0 = Bit_Ite
    primitive0 = Bit_Prim

bv_eq :: Bit n -> Bit n -> Bool
bv_eq = symapp2 P.$ \av bv ->
    if (av :: P.Bit) P.== bv
        then True
        else False

bv_show :: Bit n -> List__ Char
bv_show = symapp P.$ \av -> fromHSString (P.show (av :: P.Bit))

bv_fromInteger :: P.Integer -> Integer -> Bit n
bv_fromInteger w = symapp P.$ \v -> Bit (P.bv_make w v)

bv_add :: Bit n -> Bit n -> Bit n
bv_add = symapp2 P.$ \av bv -> Bit (av P.+ bv)

bv_sub :: Bit n -> Bit n -> Bit n
bv_sub = symapp2 P.$ \av bv -> Bit (av P.- bv)

