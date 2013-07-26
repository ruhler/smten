
{-# LANGUAGE ScopedTypeVariables #-}
module Smten.Compiled.Smten.Data.Bit0 (
    Bit, bv_eq, bv_show, bv_fromInteger, bv_add, bv_sub,
    ) where

import Prelude hiding (Bool(..))
import qualified Data.BitVector.FixedWidth as P
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf
import Smten.Runtime.Types

data Bit n =
    Bit (P.Bit n)
  | Bit_Ite Bool (Bit n) (Bit n)
  | Bit_Err ErrorString
  | Bit_Prim (Model -> Bit n) (Bit n)

instance SymbolicOf (P.Bit n) (Bit n) where
    tosym (Bit x) = x

    symapp f x = 
      case x of
        Bit b -> f b
        Bit_Ite p a b -> ite0 p (f $$ a) (f $$ b)
        Bit_Err msg -> error0 msg
        Bit_Prim r x -> primitive0 (\m -> realize m (f $$ (r m))) (f $$ x)
   
instance SmtenHS1 Bit where
    error1 = Bit_Err
    realize1 m x = 
      case x of
        Bit {} -> x
        Bit_Ite p a b -> iterealize p a b m
        Bit_Err msg -> Bit_Err (realize m msg)
        Bit_Prim r _ -> r m
    ite1 = Bit_Ite
    primitive1 = Bit_Prim

bv_eq :: forall n . (Numeric n) => Bit n -> Bit n -> Bool
bv_eq = symapp2 $ \av bv ->
    if (av :: P.Bit n) == bv
        then True
        else False

bv_show :: forall n . (Numeric n) => Bit n -> String
bv_show = symapp $ \av -> fromHSString (show (av :: P.Bit n))

bv_fromInteger :: (Numeric n) => Integer -> Bit n
bv_fromInteger = symapp (Bit . fromInteger)

bv_add :: (Numeric n) => Bit n -> Bit n -> Bit n
bv_add = symapp2 $ \av bv -> Bit (av + bv)

bv_sub :: (Numeric n) => Bit n -> Bit n -> Bit n
bv_sub = symapp2 $ \av bv -> Bit (av - bv)

