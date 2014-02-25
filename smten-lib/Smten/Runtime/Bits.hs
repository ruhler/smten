
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Add support for symbolic bit vectors to a backend which doesn't
-- already support bit vectors.
module Smten.Runtime.Bits(addBits) where

import Data.Functor
import Data.Maybe
import Data.IORef
import Data.Typeable
import qualified Data.HashTable.IO as H

import Smten.Runtime.FreeID
import Smten.Runtime.Formula.Type
import Smten.Runtime.SolverAST

data Formula exp = Exp { expr :: exp }
             | BitF { bits :: [exp] }   -- LSB first
    deriving (Typeable)

-- We remap FreeID's for each variable.
--  Bit types get mapped to   m, m+1, m+2, ..., m+w-1
--  Regular types get mapped to some m.
data VarInfo = IsBit FreeID Integer    -- idmin, width
             | IsOther FreeID          -- id

type BitVarMap e = H.BasicHashTable FreeID VarInfo
type NextID = IORef Integer

data Bits s e = Bits s (BitVarMap e) NextID

bitstodo nm = error $ "TODO: Bits support for " ++ nm

-- | Add bit-vector support to an existing solver.
addBits :: s -> IO (Bits s exp)
addBits s = do
    m <- H.new
    nid <- newIORef 0
    return (Bits s m nid)

instance (SolverAST s exp) => SolverAST (Bits s exp) (Formula exp) where
  declare (Bits s m nid) (BitT w) nm = {-# SCC "Bits_declare" #-} do
     idmin <- readIORef nid
     let idnext = idmin + w
         idmax = idnext - 1
     writeIORef nid $! idnext
     mapM (declare s BoolT) [idmin .. idmax]
     H.insert m nm (IsBit idmin w)
  declare (Bits s m nid) t nm = {-# SCC "Bits_declare" #-} do
     id <- readIORef nid
     writeIORef nid $! id+1
     declare s t id
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

  check (Bits s _ _) = check s
  cleanup (Bits s _ _) = cleanup s
        
  assert (Bits s _ _) e = assert s (expr e)

  bool (Bits s _ _) b = Exp <$> bool s b

  integer (Bits s _ _) i = Exp <$> integer s i
  bit (Bits s _ _) w v = BitF <$> mapM (bool s) (intB w v)

  var (Bits s m _) nm = {-# SCC "Bits_var" #-} do
    v <- fromJust <$> H.lookup m nm
    case v of
      IsBit idmin w -> do
        let idmax = idmin + w - 1
        BitF <$> mapM (var s) [idmin .. idmax]
      IsOther id -> Exp <$> var s id

  and_bool = {-# SCC "Bits_and" #-} bprim and_bool
  or_bool = {-# SCC "Bits_or" #-} bprim or_bool
  not_bool = {-# SCC "Bits_not" #-} uprim not_bool
    
  ite_bool (Bits s _ _) p a b = {-# SCC "Bits_ite_bool" #-} Exp <$> ite_bool s (expr p) (expr a) (expr b)
  ite_bit (Bits s _ _) p a b = {-# SCC "Bits_ite_bit" #-}
    let ites = [ite_bool s (expr p) av bv | (av, bv) <- zip (bits a) (bits b)]
    in BitF <$> sequence ites

  ite_integer (Bits s _ _) p a b = {-# SCC "Bits_ite_integer" #-} Exp <$> ite_integer s (expr p) (expr a) (expr b)
  eq_integer = bprim eq_integer
  leq_integer = bprim leq_integer
  add_integer = bprim add_integer
  sub_integer = bprim sub_integer

  -- all individual bits must be equal
  eq_bit (Bits s _ _) a b = do
    eqs <- sequence $ zipWith (eq_bool s) (bits a) (bits b)
    Exp <$> and_bools s eqs

  leq_bit bs@(Bits s _ _) a b = do
    let -- leq with MSB first
        msbleq [] [] = bool s True
        msbleq (a:as) (b:bs) = do
            a_eq_b <- eq_bool s a b
            as_leq_bs <- msbleq as bs
            ite_bool s a_eq_b as_leq_bs b
    Exp <$> msbleq (reverse (bits a)) (reverse (bits b))

  add_bit (Bits s _ _) a b = do
    ff <- bool s False
    BitF <$> add s ff (bits a) (bits b)
    
  sub_bit bs@(Bits s _ _) a b = do
    b_not <- not_bit bs b
    tt <- bool s True
    BitF <$> add s tt (bits a) (bits b_not)

  mul_bit = bitstodo "mul"

  or_bit (Bits s _ _) a b = do
    BitF <$> sequence (zipWith (or_bool s) (bits a) (bits b))

  and_bit (Bits s _ _) a b = do
    BitF <$> sequence (zipWith (and_bool s) (bits a) (bits b))

  concat_bit (Bits s _ _) a b = return (BitF $ (bits b) ++ (bits a))

  shl_bit = bitstodo "shl"
  lshr_bit = bitstodo "lshr"

  not_bit (Bits s _ _) x = BitF <$> mapM (not_bool s) (bits x)

  sign_extend_bit _ fr to x = do
    let sign = last (bits x)
    return (BitF (bits x ++ replicate (fromInteger $ to-fr) sign))

  extract_bit _ hi lo x =
    let loi = fromInteger lo
        hii = fromInteger hi
    in return (BitF (drop loi (take (hii + 1) (bits x))))

uprim :: (SolverAST s e) => (s -> e -> IO e)
      -> Bits s e -> Formula e -> IO (Formula e)
uprim f (Bits s _ _) a = Exp <$> f s (expr a)

bprim :: (SolverAST s e) => (s -> e -> e -> IO e)
      -> Bits s e -> Formula e -> Formula e -> IO (Formula e)
bprim f (Bits s _ _) a b = Exp <$> f s (expr a) (expr b)

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
add :: (SolverAST s exp) => s -> exp -> [exp] -> [exp] -> IO [exp] 
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

