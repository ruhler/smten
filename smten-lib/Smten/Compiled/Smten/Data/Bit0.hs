
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK hide #-}
module Smten.Compiled.Smten.Data.Bit0 (
    Bit, bv_eq, bv_leq, bv_show, bv_fromInteger, bv_add, bv_sub, bv_mul,
    bv_or, bv_and, bv_shl, bv_lshr, bv_not, bv_concat,
    bv_sign_extend, bv_extract, bv_width, bv_value,
    ) where

import qualified Prelude as P
import qualified Smten.Runtime.Bit as P
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf
import Smten.Runtime.Formula
import Smten.Compiled.Smten.Data.Bool0
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.GHC.TypeLits

type Bit = BitF

instance SymbolicOf P.Bit (BitF n) where
    tosym = BitF

    symapp f x = 
      case x of
        BitF b -> f b
        Bit_Ite p a b -> ite0 p (f $$ a) (f $$ b)
        _ -> P.error "symapp on non-ite symbolic bit vector"

bv_eq :: SingI Nat n -> BitF n -> BitF n -> Bool
bv_eq x = {-# SCC "PRIM_BV_EQ" #-} eq_Bit (__deNewTyDGSingI x)

bv_leq :: SingI Nat n -> BitF n -> BitF n -> Bool
bv_leq x = {-# SCC "PRIM_BV_LEQ" #-} leq_Bit (__deNewTyDGSingI x)

bv_show :: BitF n -> List__ Char
bv_show = {-# SCC "PRIM_BV_SHOW" #-} symapp P.$ \av -> fromHSString (P.show (av :: P.Bit))

bv_fromInteger :: SingI Nat n -> Integer -> BitF n
bv_fromInteger w = {-# SCC "PRIM_BV_FROMINTEGER" #-} symapp P.$ \v -> BitF (P.bv_make (__deNewTyDGSingI w) v)

bv_add :: BitF n -> BitF n -> BitF n
bv_add = {-# SCC "PRIM_BV_ADD" #-} add_Bit

bv_sub :: BitF n -> BitF n -> BitF n
bv_sub = {-# SCC "PRIM_BV_SUB" #-} sub_Bit

bv_mul :: BitF n -> BitF n -> BitF n
bv_mul = {-# SCC "PRIM_BV_MUL" #-} mul_Bit

bv_or :: BitF n -> BitF n -> BitF n
bv_or = {-# SCC "PRIM_BV_OR" #-} or_Bit

bv_and :: BitF n -> BitF n -> BitF n
bv_and = {-# SCC "PRIM_BV_AND" #-} and_Bit

bv_shl :: BitF n -> BitF n -> BitF n
bv_shl = {-# SCC "PRIM_BV_SHL" #-} shl_Bit

bv_lshr :: BitF n -> BitF n -> BitF n
bv_lshr = {-# SCC "PRIM_BV_LSHR" #-} lshr_Bit

bv_not :: BitF n -> BitF n
bv_not = {-# SCC "PRIM_BV_NOT" #-} not_Bit

bv_concat :: SingI Nat a -> Bit a -> Bit b -> BitF n
bv_concat x = {-# SCC "PRIM_BV_CONCAT" #-} concat_Bit (__deNewTyDGSingI x)

bv_sign_extend :: SingI Nat m -> SingI Nat n -> Bit m -> BitF n
bv_sign_extend mw nw = {-# SCC "PRIM_BV_SIGN_EXTEND" #-} sign_extend_Bit (__deNewTyDGSingI nw P.- __deNewTyDGSingI mw)

bv_extract :: SingI Nat m -> SingI Nat n -> Bit m -> Integer -> BitF n
bv_extract mw nw x = {-# SCC "PRIM_BV_EXTRACT" #-} symapp (\lsb -> extract_Bit (__deNewTyDGSingI mw) (lsb P.+ (__deNewTyDGSingI nw) P.- 1) lsb x)

bv_width :: SingI Nat n -> BitF n -> Integer
bv_width w _ = {-# SCC "PRIM_BV_WIDTH" #-} tosym (__deNewTyDGSingI w)

bv_value :: BitF n -> Integer
bv_value = {-# SCC "PRIM_BV_VALUE" #-} symapp (\b -> tosym (P.bv_value b))

