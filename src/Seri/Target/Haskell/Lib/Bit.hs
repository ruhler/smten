
-- | Library for supporting bit vectors in the haskell target.
module Seri.Target.Haskell.Lib.Bit(
    Bit(),
    Seri.Target.Haskell.Lib.Bit.or,
    Seri.Target.Haskell.Lib.Bit.and,
    lsh, rshl, zeroExtend,
    Seri.Target.Haskell.Lib.Bit.truncate,
    ) where

import Data.Bits

import qualified Seri.Bit as B
import Seri.Target.Haskell.Lib.Numeric

data Bit n = Bit n B.Bit

instance (N__ n) => Eq (Bit n) where
    (==) (Bit _ a) (Bit _ b) = a == b

instance (N__ n) => Num (Bit n) where
    (+) (Bit w a) (Bit _ b) = Bit w (a+b)
    (*) (Bit w a) (Bit _ b) = Bit w (a*b)
    (-) (Bit w a) (Bit _ b) = Bit w (a-b)
    fromInteger i = 
        let w = numeric 
        in Bit w (B.bv_make (valueof w) i)
    abs = error "todo: abs Bit"
    signum = error "todo: signum Bit"
    
or :: N__ n => Bit n -> Bit n -> Bit n
or (Bit w a) (Bit _ b) = Bit w (a .|. b)

and :: N__ n => Bit n -> Bit n -> Bit n
and (Bit w a) (Bit _ b) = Bit w (a .&. b)

lsh :: N__ n => Bit n -> Integer -> Bit n
lsh (Bit w a) x = Bit w (a `shiftL` fromInteger x)

rshl :: N__ n => Bit n -> Integer -> Bit n
rshl (Bit w a) x = Bit w (a `shiftR` fromInteger x)

zeroExtend :: (N__ n, N__ m) => Bit n -> Bit m
zeroExtend (Bit n a)
 = let m = numeric
   in Bit m (B.bv_zero_extend (valueof m - valueof n) a)

truncate :: (N__ n, N__ m) => Bit n -> Bit m
truncate (Bit n a)
 = let m = numeric
   in Bit m (B.bv_truncate (valueof m) a)

