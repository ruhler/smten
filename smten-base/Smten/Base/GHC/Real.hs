
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
module Smten.Base.GHC.Real (
    Ratio(..), Rational, (%), numerator, denominator, gcd,
    Integral(..),
    ) where

import GHC.Base
import GHC.Enum
import GHC.Err
import GHC.Integer
import GHC.Num (Num(..))

infixl 7 `quot`, `rem`, `div`, `mod`
infixl 7 %

data Ratio a = a :% a deriving (Eq)
type Rational = Ratio Integer

(%) :: (Integral a) => a -> a -> Ratio a
numerator :: (Integral a) => Ratio a -> a
denominator :: (Integral a) => Ratio a -> a

reduce :: (Integral a) => a -> a -> Ratio a
reduce _ 0 = ratioZeroDenominatorError
reduce x y = (x `quot` d) :% (y `quot` d)
             where d = gcd x y

x % y = reduce (x * signum y) (abs y)

numerator (x :% _) = x
denominator (_ :% y) = y

class (Num a, Ord a) => Real a where
    toRational :: a -> Rational

class (Real a, Enum a) => Integral a where
    quot :: a -> a -> a
    rem :: a -> a -> a
    div :: a -> a -> a
    mod :: a -> a -> a
    quotRem :: a -> a -> (a, a)
    divMod :: a -> a -> (a, a)
    toInteger :: a -> Integer

    {-# INLINE quot #-}
    {-# INLINE rem #-}
    {-# INLINE div #-}
    {-# INLINE mod #-}
    n `quot` d          =  q  where (q,_) = quotRem n d
    n `rem` d           =  r  where (_,r) = quotRem n d
    n `div` d           =  q  where (q,_) = divMod n d
    n `mod` d           =  r  where (_,r) = divMod n d

    divMod n d          =  if signum r == negate (signum d) then (q-1, r+d) else qr
                           where qr@(q,r) = quotRem n d

instance Real Int where
   toRational x = toInteger x :% 1

instance Integral Int where
    toInteger (I# i) = smallInteger i

    a `quot` b
     | b == 0                     = divZeroError
     | b == (-1) && a == minBound = overflowError -- Note [Order of tests]
                                                  -- in GHC.Int
     | otherwise                  =  a `quotInt` b

    a `rem` b
     | b == 0                     = divZeroError
       -- The quotRem CPU instruction fails for minBound `quotRem` -1,
       -- but minBound `rem` -1 is well-defined (0). We therefore
       -- special-case it.
     | b == (-1)                  = 0
     | otherwise                  =  a `remInt` b

    a `div` b
     | b == 0                     = divZeroError
     | b == (-1) && a == minBound = overflowError -- Note [Order of tests]
                                                  -- in GHC.Int
     | otherwise                  =  a `divInt` b

    a `mod` b
     | b == 0                     = divZeroError
       -- The divMod CPU instruction fails for minBound `divMod` -1,
       -- but minBound `mod` -1 is well-defined (0). We therefore
       -- special-case it.
     | b == (-1)                  = 0
     | otherwise                  =  a `modInt` b

    a `quotRem` b
     | b == 0                     = divZeroError
       -- Note [Order of tests] in GHC.Int
     | b == (-1) && a == minBound = (overflowError, 0)
     | otherwise                  =  a `quotRemInt` b

    a `divMod` b
     | b == 0                     = divZeroError
       -- Note [Order of tests] in GHC.Int
     | b == (-1) && a == minBound = (overflowError, 0)
     | otherwise                  =  a `divModInt` b

instance Real Integer where
    toRational x = x :% 1

instance Integral Integer where
    toInteger n = n

    _ `quot` 0 = divZeroError
    n `quot` d = n `quotInteger` d

    _ `rem` 0 = divZeroError
    n `rem`  d = n `remInteger`  d

    _ `div` 0 = divZeroError
    n `div` d = n `divInteger` d

    _ `mod` 0 = divZeroError
    n `mod`  d = n `modInteger`  d

    _ `divMod` 0 = divZeroError
    a `divMod` b = case a `divModInteger` b of
                   (# x, y #) -> (x, y)

    _ `quotRem` 0 = divZeroError
    a `quotRem` b = case a `quotRemInteger` b of
                    (# q, r #) -> (q, r)

gcd :: (Integral a) => a -> a -> a
gcd x y = gcd' (abs x) (abs y)
          where gcd' a 0 = a
                gcd' a b = gcd' b (a `rem` b)
