
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# OPTIONS_GHC -fprof-auto-top #-}

module Smten.Runtime.Formula.BitF (
    BitF(..), bitF, bit_eqF, bit_leqF, bit_addF, bit_subF, bit_mulF,
    bit_orF, bit_andF, bit_shlF, bit_lshrF, bit_notF, bit_concatF,
    bit_sign_extendF, bit_extractF,
    finite_BitF, parts_BitF, var_BitF, ite_BitF, unreachable_BitF,
  ) where

import GHC.TypeLits

import Smten.Runtime.Bit
import Smten.Runtime.FreeID
import Smten.Runtime.Formula.BoolF
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula.PartialF

newtype BitF (n :: Nat) = BitF (PartialF BitFF)

instance Finite BitFF where
    ite_finite = ite_BitFF
    unreachable_finite = Unreachable_BitFF

bitF :: Bit -> BitF n
bitF x = BitF $ finitePF (bitFF x)

var_BitF :: Integer -> FreeID -> BitF n
var_BitF w nm = BitF $ finitePF (var_BitFF w nm)

bit_eqF :: BitF n -> BitF n -> BoolF
bit_eqF (BitF a) (BitF b) = BoolF $ binaryPF eq_BitFF a b

bit_leqF :: BitF n -> BitF n -> BoolF
bit_leqF (BitF a) (BitF b) = BoolF $ binaryPF leq_BitFF a b

bit_addF :: BitF n -> BitF n -> BitF n
bit_addF (BitF a) (BitF b) = BitF $ binaryPF add_BitFF a b

bit_subF :: BitF n -> BitF n -> BitF n
bit_subF (BitF a) (BitF b) = BitF $ binaryPF sub_BitFF a b

bit_mulF :: BitF n -> BitF n -> BitF n
bit_mulF (BitF a) (BitF b) = BitF $ binaryPF mul_BitFF a b

bit_orF :: BitF n -> BitF n -> BitF n
bit_orF (BitF a) (BitF b) = BitF $ binaryPF bit_orFF a b

bit_andF :: BitF n -> BitF n -> BitF n
bit_andF (BitF a) (BitF b) = BitF $ binaryPF bit_andFF a b

bit_shlF :: Integer -> BitF n -> BitF n -> BitF n
bit_shlF w (BitF a) (BitF b) = BitF $ binaryPF (bit_shlFF w) a b

bit_lshrF :: Integer -> BitF n -> BitF n -> BitF n
bit_lshrF w (BitF a) (BitF b) = BitF $ binaryPF (bit_lshrFF w) a b

bit_concatF :: BitF a -> BitF b -> BitF n
bit_concatF (BitF a) (BitF b) = BitF $ binaryPF bit_concatFF a b

bit_notF :: BitF n -> BitF n
bit_notF (BitF a) = BitF $ unaryPF bit_notFF a

bit_sign_extendF :: Integer -> Integer -> BitF m -> BitF n
bit_sign_extendF fr to (BitF a) = BitF $ unaryPF (bit_sign_extendFF fr to) a

bit_extractF :: Integer -> Integer -> BitF m -> BitF n
bit_extractF hi lo (BitF a) = BitF $ unaryPF (bit_extractFF hi lo) a

finite_BitF :: BitFF -> BitF n
finite_BitF x = BitF (finitePF x)

parts_BitF :: BitF n -> (BoolFF, BitFF, BitF n)
parts_BitF (BitF (PartialF p a b_)) = (p, a, BitF b_)

ite_BitF :: BoolF -> BitF n -> BitF n -> BitF n
ite_BitF (BoolF p) (BitF a) (BitF b) = BitF $ itePF p a b

unreachable_BitF :: BitF n
unreachable_BitF = BitF unreachablePF

