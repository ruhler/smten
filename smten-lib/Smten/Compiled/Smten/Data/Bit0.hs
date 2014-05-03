
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
import Smten.Runtime.SymbolicOf
import Smten.Runtime.Formula
import Smten.Runtime.Formula.Finite
import Smten.Compiled.GHC.Types
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.GHC.TypeLits

type Bit = BitF

instance SymbolicOf P.Bit (BitF n) where
    tosym = bitF

    symapp f x = 
      case parts_BitF x of
        (TrueFF, BitFF b, _) -> f b
        _ -> P.error "TODO: symapp symbolic BitF"

bv_eq :: BitF n -> BitF n -> Bool
bv_eq = {-# SCC "PRIM_BV_EQ" #-} bit_eqF

bv_leq :: BitF n -> BitF n -> Bool
bv_leq = {-# SCC "PRIM_BV_LEQ" #-} bit_leqF

bv_show :: BitF n -> List__ Char
bv_show = {-# SCC "PRIM_BV_SHOW" #-} symapp P.$ \av -> fromHSString (P.show (av :: P.Bit))

bv_fromInteger :: KnownNat n -> Integer -> BitF n
bv_fromInteger w = {-# SCC "PRIM_BV_FROMINTEGER" #-} symapp P.$ \v -> bitF (P.bv_make (knownNatVal w) v)

bv_add :: BitF n -> BitF n -> BitF n
bv_add = {-# SCC "PRIM_BV_ADD" #-} bit_addF

bv_sub :: BitF n -> BitF n -> BitF n
bv_sub = {-# SCC "PRIM_BV_SUB" #-} bit_subF

bv_mul :: BitF n -> BitF n -> BitF n
bv_mul = {-# SCC "PRIM_BV_MUL" #-} bit_mulF

bv_or :: BitF n -> BitF n -> BitF n
bv_or = {-# SCC "PRIM_BV_OR" #-} bit_orF

bv_and :: BitF n -> BitF n -> BitF n
bv_and = {-# SCC "PRIM_BV_AND" #-} bit_andF

bv_shl :: KnownNat n -> BitF n -> BitF n -> BitF n
bv_shl w = {-# SCC "PRIM_BV_SHL" #-} bit_shlF (knownNatVal w)

bv_lshr :: KnownNat n -> BitF n -> BitF n -> BitF n
bv_lshr w = {-# SCC "PRIM_BV_LSHR" #-} bit_lshrF (knownNatVal w)

bv_not :: BitF n -> BitF n
bv_not = {-# SCC "PRIM_BV_NOT" #-} bit_notF

bv_concat :: Bit a -> Bit b -> BitF n
bv_concat = {-# SCC "PRIM_BV_CONCAT" #-} bit_concatF

bv_sign_extend :: KnownNat m -> KnownNat n -> Bit m -> BitF n
bv_sign_extend mw nw = {-# SCC "PRIM_BV_SIGN_EXTEND" #-} bit_sign_extendF (knownNatVal mw) (knownNatVal nw)

bv_extract :: KnownNat n -> Bit m -> Integer -> BitF n
bv_extract nw x = {-# SCC "PRIM_BV_EXTRACT" #-} symapp (\lsb -> bit_extractF (lsb P.+ (knownNatVal nw) P.- 1) lsb x)

bv_width :: KnownNat n -> BitF n -> Integer
bv_width w _ = {-# SCC "PRIM_BV_WIDTH" #-} tosym (knownNatVal w)

bv_value :: BitF n -> Integer
bv_value = {-# SCC "PRIM_BV_VALUE" #-} symapp (\b -> tosym (P.bv_value b))

