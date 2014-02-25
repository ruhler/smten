
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Add limited support for symbolic Integers to a backend which doesn't
-- already support integers.
module Smten.Runtime.Integers (addIntegers) where

import Data.Functor
import Data.Typeable

import Smten.Runtime.SolverAST

data Formula exp = Exp { expr :: exp }
             | IntegerF { ints :: [(exp, Integer)] }
    deriving (Typeable)

newtype Integers s = Integers s

-- | Add Integer support to an existing solver.
addIntegers :: s -> IO (Integers s)
addIntegers s = return (Integers s)

instance (SolverAST s exp) => SolverAST (Integers s) (Formula exp) where
  declare (Integers s) t nm = declare s t nm
  
  getBoolValue (Integers s) nm = getBoolValue s nm
  getIntegerValue (Integers s) nm = getIntegerValue s nm
  getBitVectorValue (Integers s) w nm = getBitVectorValue s w nm

  check (Integers s) = check s
  cleanup (Integers s) = cleanup s
        
  assert (Integers s) e = assert s (expr e)

  bool (Integers s) b = Exp <$> bool s b

  integer (Integers s) i = do
    tt <- bool s True
    return (IntegerF [(tt, i)])

  bit (Integers s) w v = Exp <$> bit s w v

  var (Integers s) nm = Exp <$> var s nm

  and_bool = bprim and_bool
  or_bool = bprim or_bool
  not_bool = uprim not_bool
    
  ite_bool (Integers s) p a b = Exp <$> ite_bool s (expr p) (expr a) (expr b)
  ite_bit (Integers s) p a b = Exp <$> ite_bit s (expr p) (expr a) (expr b)

  ite_integer (Integers s) p a b = do
    let --join :: exp -> (exp, Integer) -> IO (exp, Integer)
        join p (a, v) = do
          pa <- and_bool s p a
          return (pa, v)
    not_p <- not_bool s (expr p)
    a' <- mapM (join (expr p)) (ints a)
    b' <- mapM (join not_p) (ints b)
    return $ IntegerF (a' ++ b')

  eq_integer = ibprim (==)
  leq_integer = ibprim (<=)
  add_integer = iiprim (+)
  sub_integer = iiprim (-)

  eq_bit = bprim eq_bit
  leq_bit = bprim leq_bit
  add_bit = bprim add_bit
  sub_bit = bprim sub_bit
  mul_bit = bprim mul_bit
  or_bit = bprim or_bit
  and_bit = bprim and_bit
  concat_bit = bprim concat_bit
  shl_bit s x = bprim (\s' -> shl_bit s' x) s
  lshr_bit s x = bprim (\s' -> lshr_bit s' x) s
  not_bit = uprim not_bit
  sign_extend_bit (Integers s) fr to x = Exp <$> sign_extend_bit s fr to (expr x)
  extract_bit (Integers s) hi lo x = Exp <$> extract_bit s hi lo (expr x)

uprim :: (SolverAST s e) => (s -> e -> IO e)
      -> Integers s -> Formula e -> IO (Formula e)
uprim f (Integers s) a = Exp <$> f s (expr a)

bprim :: (SolverAST s e) => (s -> e -> e -> IO e)
      -> Integers s -> Formula e -> Formula e -> IO (Formula e)
bprim f (Integers s) a b = Exp <$> f s (expr a) (expr b)

ibprim :: forall s e . (SolverAST s e) => (Integer -> Integer -> Bool) 
       -> Integers s -> Formula e -> Formula e -> IO (Formula e)
ibprim f (Integers s) a b = do
  let join :: (e, Integer) -> (e, Integer) -> IO e
      join (pa, va) (pb, vb) = do
        pab <- and_bool s pa pb
        v <- bool s (f va vb)
        and_bool s pab v

  vals <- sequence [join ax bx | ax <- ints a, bx <- ints b]
  Exp <$> or_bools s vals

iiprim :: forall s e . (SolverAST s e) => (Integer -> Integer -> Integer) 
       -> Integers s -> Formula e -> Formula e -> IO (Formula e)
iiprim f (Integers s) a b = do
  let join :: (e, Integer) -> (e, Integer) -> IO (e, Integer)
      join (pa, va) (pb, vb) = do
        pab <- and_bool s pa pb
        let vab = f va vb
        return (pab, vab)
  IntegerF <$> sequence [join ax bx | ax <- ints a, bx <- ints b]

