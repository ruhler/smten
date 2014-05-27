
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Add support for symbolic bit vectors to a backend which doesn't
-- already support bit vectors.
module Smten.Runtime.Bits(Bits, addBits) where

import Control.Monad
import Data.Functor
import Data.Maybe
import qualified Data.HashTable.IO as H

import Smten.Runtime.Bit
import Smten.Runtime.FreeID
import Smten.Runtime.Formula.Type
import Smten.Runtime.Model
import Smten.Runtime.SolverAST

type BitE b = [b]   -- List of bits, LSB first

-- Map from bit vector variable to the generated bool variables.
type BitVarMap = H.BasicHashTable FreeID [FreeID]

data Bits s b i v = Bits s BitVarMap

bitstodo nm = error $ "TODO: Bits support for " ++ nm

-- | Add bit-vector support to an existing solver.
addBits :: s -> IO (Bits s b i v)
addBits s = do
    m <- H.new
    return (Bits s m)

instance (SolverAST s b i v) => SolverAST (Bits s b i v) b i (BitE b) where
  declare_bit (Bits s m) w nm = do
     nms <- replicateM (fromInteger w) fresh 
     mapM (declare_bool s) nms
     H.insert m nm nms

  declare_integer (Bits s m) nm = declare_integer s nm
  declare_bool (Bits s m) nm = declare_bool s nm
  
  getBoolValue (Bits s m) nm = getBoolValue s nm
  getIntegerValue (Bits s m) nm = getIntegerValue s nm

  getBitVectorValue (Bits s m) w nm = do
     nms <- fromJust <$> H.lookup m nm
     bits <- mapM (getBoolValue s) nms
     return (valB bits)

  getModel (Bits s m) vars =
    let getValue (nm, BoolT) = BoolA <$> getBoolValue s nm
        getValue (nm, IntegerT) = IntegerA <$> getIntegerValue s nm
        getValue (nm, BitT w) = do
           nms <- fromJust <$> H.lookup m nm
           bits <- mapM (getBoolValue s) nms
           return (BitA $ bv_make w (valB bits))
    in mapM getValue vars

  check (Bits s _) = check s
  cleanup (Bits s _) = cleanup s
        
  assert (Bits s _) e = assert s e

  bool (Bits s _) b = bool s b

  integer (Bits s _) i = integer s i
  bit (Bits s _) w v = mapM (bool s) (intB w v)

  var_bool (Bits s m) nm = var_bool s nm
  var_integer (Bits s m) nm = var_integer s nm
  var_bit (Bits s m) w nm = do
    nms <- fromJust <$> H.lookup m nm
    mapM (var_bool s) nms

  and_bool = bprim and_bool
  or_bool = bprim or_bool
  not_bool (Bits s _) a = not_bool s a
    
  ite_bool (Bits s _) p a b = ite_bool s p a b
  ite_bit (Bits s _) p a b = 
    let ites = [ite_bool s p av bv | (av, bv) <- zip a b]
    in sequence ites

  ite_integer (Bits s _) p a b = ite_integer s p a b
  eq_integer = bprim eq_integer
  leq_integer = bprim leq_integer
  add_integer = bprim add_integer
  sub_integer = bprim sub_integer

  -- all individual bits must be equal
  eq_bit (Bits s _) a b = do
    eqs <- zipWithM (eq_bool s) a b
    and_bools s eqs

  leq_bit bs@(Bits s _) a b = do
    let -- leq with MSB first
        msbleq [] [] = bool s True
        msbleq (a:as) (b:bs) = do
            a_eq_b <- eq_bool s a b
            as_leq_bs <- msbleq as bs
            ite_bool s a_eq_b as_leq_bs b
    msbleq (reverse a) (reverse b)

  add_bit (Bits s _) a b = do
    ff <- bool s False
    add s ff a b
    
  sub_bit (Bits s _) a b = do
    b_not <- mapM (not_bool s) b
    tt <- bool s True
    add s tt a b_not

  mul_bit = bitstodo "mul"
  sdiv_bit = bitstodo "sdiv"
  srem_bit = bitstodo "srem"
  smod_bit = bitstodo "smod"
  udiv_bit = bitstodo "udiv"
  urem_bit = bitstodo "urem"

  or_bit (Bits s _) a b = zipWithM (or_bool s) a b
  and_bit (Bits s _) a b = zipWithM (and_bool s) a b

  concat_bit (Bits s _) a b = return (b ++ a)

  shl_bit = bitstodo "shl"
  lshr_bit = bitstodo "lshr"

  not_bit (Bits s _) x = mapM (not_bool s) x

  sign_extend_bit _ fr to x = do
    let sign = last x
    return (x ++ replicate (fromInteger $ to-fr) sign)

  extract_bit _ hi lo x =
    let loi = fromInteger lo
        hii = fromInteger hi
    in return (drop loi (take (hii + 1) x))

bprim :: (s -> a -> b -> IO c) -> Bits s d i v -> a -> b -> IO c
bprim f (Bits s _) a b = f s a b

-- Return the integer value of a bit vector.
-- takes the bit vector with lsb first.
valB :: [Bool] -> Integer
valB [] = 0
valB (b:xs) = (if b then 1 else 0) + 2 * (valB xs)

intB :: Integer -> Integer -> [Bool]
intB 0 _ = []
intB w x = case x `quotRem` 2 of
                (x2, b) -> (b == 1) : intB (w-1) x2

-- add s cin a b
--   Add the bit vectors a and b with the given carry in.
add :: (SolverAST s b i v) => s -> b -> [b] -> [b] -> IO [b] 
add s _ [] [] = return []
add s c (a:as) (b:bs) = do
   xor_ab <- xor_bool s a b
   z <- xor_bool s c xor_ab
   ca <- and_bool s c a
   cb <- and_bool s c b
   ab <- and_bool s a b
   c' <- or_bools s [ca, cb, ab]
   tl <- add s c' as bs
   return (z : tl)

