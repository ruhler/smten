
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Smten.Compiled.Smten.Data.Bit0 (
    Bit, bv_eq, bv_leq, bv_show, bv_fromInteger, bv_add, bv_sub, bv_mul,
    bv_or, bv_and, bv_shl, bv_lshr, bv_not, bv_concat,
    bv_sign_extend, bv_extract, bv_width, bv_value,
    ) where

import qualified Prelude as P
import qualified Smten.Runtime.Bit as P
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf
import Smten.Runtime.Types
import Smten.Compiled.Smten.Smten.Base

instance SymbolicOf P.Bit (Bit n) where
    tosym = Bit

    symapp f x = 
      case x of
        Bit b -> f b
        Bit_Ite p a b -> ite0 p (f $$ a) (f $$ b)
        Bit_Err msg -> error0 msg
        Bit_Prim r x -> primitive0 (\m -> realize m (f $$ (r m))) (f $$ x)
        _ -> P.error "symapp on non-ite symbolic bit vector"

bv_eq :: Bit n -> Bit n -> Bool
bv_eq = eq_Bit

bv_leq :: Bit n -> Bit n -> Bool
bv_leq = leq_Bit

bv_show :: Bit n -> List__ Char
bv_show = symapp P.$ \av -> fromHSString (P.show (av :: P.Bit))

bv_fromInteger :: P.Integer -> Integer -> Bit n
bv_fromInteger w = symapp P.$ \v -> Bit (P.bv_make w v)

bv_add :: Bit n -> Bit n -> Bit n
bv_add = add_Bit

bv_sub :: Bit n -> Bit n -> Bit n
bv_sub = sub_Bit

bv_mul :: Bit n -> Bit n -> Bit n
bv_mul = mul_Bit

bv_or :: Bit n -> Bit n -> Bit n
bv_or = or_Bit

bv_and :: Bit n -> Bit n -> Bit n
bv_and = and_Bit

bv_shl :: Bit n -> Bit n -> Bit n
bv_shl = shl_Bit

bv_lshr :: Bit n -> Bit n -> Bit n
bv_lshr = lshr_Bit

bv_not :: Bit n -> Bit n
bv_not = not_Bit

bv_concat :: Bit a -> Bit b -> Bit n
bv_concat = concat_Bit

bv_sign_extend :: P.Integer -> P.Integer -> Bit m -> Bit n
bv_sign_extend mw nw = sign_extend_Bit (nw P.- mw)

bv_extract :: P.Integer -> Bit m -> Integer -> Bit n
bv_extract nw x = symapp (\lsb -> extract_Bit (lsb P.+ nw P.- 1) lsb x)

bv_width :: P.Integer -> Bit n -> Integer
bv_width w _ = tosym w

bv_value :: Bit n -> Integer
bv_value = symapp (\b -> tosym (P.bv_value b))

