
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Add limited support for symbolic Integers to a backend which doesn't
-- already support integers.
module Smten.Runtime.Integers (addIntegers) where

import Smten.Runtime.SolverAST

newtype Integers s b i v = Integers s

-- | Add Integer support to an existing solver.
addIntegers :: s -> IO (Integers s b i v)
addIntegers s = return (Integers s)

instance (SolverAST s b i v) => SolverAST (Integers s b i v) b [(b, Integer)] v  where
  declare_bool (Integers s) nm = declare_bool s nm
  declare_integer (Integers s) nm = error "TODO: support free integers in Integers wrapper"
  declare_bit (Integers s) w nm = declare_bit s w nm
  
  getBoolValue (Integers s) nm = getBoolValue s nm
  getIntegerValue (Integers s) nm = getIntegerValue s nm
  getBitVectorValue (Integers s) w nm = getBitVectorValue s w nm
  getModel (Integers s) vars = getModel s vars

  check (Integers s) = check s
  cleanup (Integers s) = cleanup s
        
  assert (Integers s) e = assert s e

  bool (Integers s) b = bool s b

  integer (Integers s) i = do
    tt <- bool s True
    return [(tt, i)]

  bit (Integers s) w v = bit s w v

  var_bool (Integers s) nm = var_bool s nm
  var_bit (Integers s) w nm = var_bit s w nm
  var_integer (Integers s) nm = error "TODO: support integer variables in Integers wrapper"

  and_bool (Integers s) a b = and_bool s a b
  or_bool (Integers s) a b = or_bool s a b
  not_bool (Integers s) a = not_bool s a
    
  ite_bool (Integers s) p a b = ite_bool s p a b
  ite_bit (Integers s) p a b = ite_bit s p a b

  ite_integer (Integers s) p a b = do
    let join p (a, v) = do
          pa <- and_bool s p a
          return (pa, v)
    not_p <- not_bool s p
    a' <- mapM (join p) a
    b' <- mapM (join not_p) b
    return (a' ++ b')

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
  not_bit (Integers s) a = not_bit s a
  sign_extend_bit (Integers s) fr to x = sign_extend_bit s fr to x
  extract_bit (Integers s) hi lo x = extract_bit s hi lo x

bprim :: (s -> a -> b -> IO c) -> Integers s d i v -> a -> b -> IO c
bprim f (Integers s) a b = f s a b

ibprim :: forall s b i v . (SolverAST s b i v) => (Integer -> Integer -> Bool) 
       -> Integers s b i v -> [(b, Integer)] -> [(b, Integer)] -> IO b
ibprim f (Integers s) a b = do
  let join :: (b, Integer) -> (b, Integer) -> IO b
      join (pa, va) (pb, vb) = do
        pab <- and_bool s pa pb
        v <- bool s (f va vb)
        and_bool s pab v

  vals <- sequence [join ax bx | ax <- a, bx <- b]
  or_bools s vals

iiprim :: forall s b i v . (SolverAST s b i v) => (Integer -> Integer -> Integer) 
       -> Integers s b i v -> [(b, Integer)] -> [(b, Integer)] -> IO [(b, Integer)]
iiprim f (Integers s) a b = do
  let join :: (b, Integer) -> (b, Integer) -> IO (b, Integer)
      join (pa, va) (pb, vb) = do
        pab <- and_bool s pa pb
        let vab = f va vb
        return (pab, vab)
  sequence [join ax bx | ax <- a, bx <- b]

