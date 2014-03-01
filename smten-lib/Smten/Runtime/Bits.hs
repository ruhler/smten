
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Add support for symbolic bit vectors to a backend which doesn't
-- already support bit vectors.
module Smten.Runtime.Bits(addBits) where

import Data.Functor
import Data.Maybe
import Data.IORef
import qualified Data.HashTable.IO as H

import Smten.Runtime.Bit
import Smten.Runtime.FreeID
import Smten.Runtime.Formula.Type
import Smten.Runtime.Model
import Smten.Runtime.SolverAST

type BitE b = [b]   -- List of bits, LSB first

-- We remap FreeID's for each variable.
--  Bit types get mapped to   m, m+1, m+2, ..., m+w-1
--  Regular types get mapped to some m.
data VarInfo = IsBit FreeID Integer    -- idmin, width
             | IsOther FreeID          -- id

type BitVarMap = H.BasicHashTable FreeID VarInfo
type NextID = IORef Integer

data Bits s b i v = Bits s BitVarMap NextID

bitstodo nm = error $ "TODO: Bits support for " ++ nm

-- | Add bit-vector support to an existing solver.
addBits :: s -> IO (Bits s b i v)
addBits s = do
    m <- H.new
    nid <- newIORef 0
    return (Bits s m nid)

instance (SolverAST s b i v) => SolverAST (Bits s b i v) b i (BitE b) where
  declare_bit (Bits s m nid) w nm = do
     idmin <- readIORef nid
     let idnext = idmin + w
         idmax = idnext - 1
     writeIORef nid $! idnext
     mapM (declare_bool s) [idmin .. idmax]
     H.insert m nm (IsBit idmin w)

  declare_integer (Bits s m nid) nm = do
     id <- readIORef nid
     writeIORef nid $! id+1
     declare_integer s id
     H.insert m nm (IsOther id)

  declare_bool (Bits s m nid) nm = do
     id <- readIORef nid
     writeIORef nid $! id+1
     declare_bool s id
     H.insert m nm (IsOther id)
  
  getBoolValue (Bits s m _) nm = do
     IsOther id <- fromJust <$> H.lookup m nm
     getBoolValue s id

  getIntegerValue (Bits s m _) nm = do
     IsOther id <- fromJust <$> H.lookup m nm
     getIntegerValue s id

  getBitVectorValue (Bits s m _) w nm = do
     IsBit idmin _ <- fromJust <$> H.lookup m nm
     let idmax = idmin + w - 1
     bits <- mapM (getBoolValue s) [idmin .. idmax]
     return (valB bits)

  getModel (Bits s m _) vars =
    let getValue (nm, BoolT) = do
           IsOther id <- fromJust <$> H.lookup m nm
           BoolA <$> getBoolValue s id
        getValue (nm, IntegerT) = do
           IsOther id <- fromJust <$> H.lookup m nm
           IntegerA <$> getIntegerValue s id
        getValue (nm, BitT w) = do
           IsBit idmin _ <- fromJust <$> H.lookup m nm
           let idmax = idmin + w - 1
           bits <- mapM (getBoolValue s) [idmin .. idmax]
           return (BitA $ bv_make w (valB bits))
    in mapM getValue vars

  check (Bits s _ _) = check s
  cleanup (Bits s _ _) = cleanup s
        
  assert (Bits s _ _) e = assert s e

  bool (Bits s _ _) b = bool s b

  integer (Bits s _ _) i = integer s i
  bit (Bits s _ _) w v = mapM (bool s) (intB w v)

  var_bool (Bits s m _) nm =  do
    IsOther id <- fromJust <$> H.lookup m nm
    var_bool s id

  var_integer (Bits s m _) nm =  do
    IsOther id <- fromJust <$> H.lookup m nm
    var_integer s id

  var_bit (Bits s m _) w nm = do
    IsBit idmin w <- fromJust <$> H.lookup m nm
    let idmax = idmin + w - 1
    mapM (var_bool s) [idmin .. idmax]

  and_bool = bprim and_bool
  or_bool = bprim or_bool
  not_bool (Bits s _ _) a = not_bool s a
    
  ite_bool (Bits s _ _) p a b = ite_bool s p a b
  ite_bit (Bits s _ _) p a b = 
    let ites = [ite_bool s p av bv | (av, bv) <- zip a b]
    in sequence ites

  ite_integer (Bits s _ _) p a b = ite_integer s p a b
  eq_integer = bprim eq_integer
  leq_integer = bprim leq_integer
  add_integer = bprim add_integer
  sub_integer = bprim sub_integer

  -- all individual bits must be equal
  eq_bit (Bits s _ _) a b = do
    eqs <- sequence $ zipWith (eq_bool s) a b
    and_bools s eqs

  leq_bit bs@(Bits s _ _) a b = do
    let -- leq with MSB first
        msbleq [] [] = bool s True
        msbleq (a:as) (b:bs) = do
            a_eq_b <- eq_bool s a b
            as_leq_bs <- msbleq as bs
            ite_bool s a_eq_b as_leq_bs b
    msbleq (reverse a) (reverse b)

  add_bit (Bits s _ _) a b = do
    ff <- bool s False
    add s ff a b
    
  sub_bit (Bits s _ _) a b = do
    b_not <- mapM (not_bool s) b
    tt <- bool s True
    add s tt a b_not

  mul_bit = bitstodo "mul"

  or_bit (Bits s _ _) a b = do
    sequence (zipWith (or_bool s) a b)

  and_bit (Bits s _ _) a b = do
    sequence (zipWith (and_bool s) a b)

  concat_bit (Bits s _ _) a b = return (b ++ a)

  shl_bit = bitstodo "shl"
  lshr_bit = bitstodo "lshr"

  not_bit (Bits s _ _) x = mapM (not_bool s) x

  sign_extend_bit _ fr to x = do
    let sign = last x
    return (x ++ replicate (fromInteger $ to-fr) sign)

  extract_bit _ hi lo x =
    let loi = fromInteger lo
        hii = fromInteger hi
    in return (drop loi (take (hii + 1) x))

bprim :: (s -> a -> b -> IO c) -> Bits s d i v -> a -> b -> IO c
bprim f (Bits s _ _) a b = f s a b

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

