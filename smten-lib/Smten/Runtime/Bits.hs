
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Add support for symbolic bit vectors to a backend which doesn't
-- already support bit vectors.
module Smten.Runtime.Bits(addBits) where

import Data.Functor
import qualified Data.HashTable.IO as H

import qualified Smten.Runtime.Types as S
import Smten.Runtime.SolverAST

data Formula exp = Exp { expr :: exp }
             | BitF { bits :: [exp] }

type BitVarMap e = H.BasicHashTable String (Formula e)

data Bits s e = Bits s (BitVarMap e)

bitstodo nm = error $ "TODO: Bits support for " ++ nm

addBits :: (SolverAST s exp) => s -> IO (Bits s exp)
addBits s = do
    m <- H.new
    return (Bits s m)

-- Get the name of the ith bit of bitvector with given name.
-- Bit 0 is the least significant bit.
bitnm :: String -> Integer -> String
bitnm x i = x ++ "~bit" ++ show i

-- Get all the bit names in order from lsb up to msb for a bitvector with
-- given name and width.
bitnms :: Integer -> String -> [String]
bitnms w x = [bitnm x i | i <- [0..w-1]]

instance (SolverAST s exp) => SolverAST (Bits s exp) (Formula exp) where
  declare (Bits s m) (S.BitT w) nm = do
     mapM (declare s S.BoolT) (bitnms w nm)
     bitv <- BitF <$> mapM (var s) (bitnms w nm)
     H.insert m nm bitv
  declare (Bits s _) t nm = declare s t nm
  
  getBoolValue (Bits s _) nm = getBoolValue s nm
  getIntegerValue (Bits s _) nm = getIntegerValue s nm
  getBitVectorValue (Bits s _) w nm = do
     bits <- mapM (getBoolValue s) (bitnms w nm)
     return (valB bits)

  check (Bits s _) = check s
  cleanup (Bits s _) = cleanup s
        
  assert (Bits s _) e = assert s (expr e)

  bool (Bits s _) b = Exp <$> bool s b

  integer (Bits s _) i = Exp <$> integer s i
  bit (Bits s _) w v = BitF <$> mapM (bool s) (intB w v)

  var (Bits s m) nm = do
    mv <- H.lookup m nm
    case mv of
      Just v -> return v
      _ -> Exp <$> var s nm

  and_bool = bprim and_bool
  or_bool = bprim or_bool
  not_bool = uprim not_bool
    
  ite_bool (Bits s _) p a b = Exp <$> ite_bool s (expr p) (expr a) (expr b)
  ite_bit (Bits s _) p a b =
    let ites = [ite_bool s (expr p) av bv | (av, bv) <- zip (bits a) (bits b)]
    in BitF <$> sequence ites

  ite_integer (Bits s _) p a b = Exp <$> ite_integer s (expr p) (expr a) (expr b)
  eq_integer = bprim eq_integer
  leq_integer = bprim leq_integer
  add_integer = bprim add_integer
  sub_integer = bprim sub_integer

  -- all individual bits must be equal
  eq_bit (Bits s _) a b = do
    eqs <- sequence $ zipWith (eq_bool s) (bits a) (bits b)
    Exp <$> andN s eqs

  leq_bit = bitstodo "leq"
  add_bit = bitstodo "add"
  sub_bit = bitstodo "sub"
  mul_bit = bitstodo "mul"

  or_bit (Bits s _) a b = do
    BitF <$> sequence [or_bool s av bv | (av, bv) <- zip (bits a) (bits b)]

  and_bit (Bits s _) a b = do
    BitF <$> sequence [and_bool s av bv | (av, bv) <- zip (bits a) (bits b)]

  concat_bit (Bits s _) a b = return (BitF $ (bits b) ++ (bits a))

  shl_bit = bitstodo "shl"
  lshr_bit = bitstodo "lshr"

  not_bit (Bits s _) x = BitF <$> mapM (not_bool s) (bits x)

  sign_extend_bit = bitstodo "sign_extend"
  extract_bit _ hi lo x =
    let loi = fromInteger lo
        hii = fromInteger hi
    in return (BitF (drop loi (take hii (bits x))))

eq_bool :: (SolverAST s e) => s -> e -> e -> IO e
eq_bool s a b = do
    notb <- not_bool s b
    ite_bool s a b notb

uprim :: (SolverAST s e) => (s -> e -> IO e)
      -> Bits s e -> Formula e -> IO (Formula e)
uprim f (Bits s _) a = Exp <$> f s (expr a)

bprim :: (SolverAST s e) => (s -> e -> e -> IO e)
      -> Bits s e -> Formula e -> Formula e -> IO (Formula e)
bprim f (Bits s _) a b = Exp <$> f s (expr a) (expr b)

-- Return the integer value of a bit vector.
-- takes the bit vector with lsb first.
valB :: [Bool] -> Integer
valB [] = 0
valB (b:xs) = (if b then 1 else 0) + 2 * (valB xs)

intB :: Integer -> Integer -> [Bool]
intB 0 _ = []
intB w x = case x `quotRem` 2 of
                (x2, b) -> (b == 1) : intB (w-1) x2

andN :: (SolverAST s e) => s -> [e] -> IO e
andN s [] = bool s True
andN s [x] = return x
andN s (x:xs) = do
  xs' <- andN s xs
  and_bool s x xs'

