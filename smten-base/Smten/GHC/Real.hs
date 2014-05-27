
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.GHC.Real (
    Integral(..),
    ) where

-- Note: this module is hardwired in the smten plugin to generate code to
-- Smten.Compiled.GHC.Real instead of Smten.Compiled.Smten.GHC.Real

import GHC.Base
import GHC.Enum
import GHC.Err
import Smten.GHC.Integer.Type
import Smten.Smten.Integer
import GHC.Num (Num(..))

data Ratio a = a :% a deriving (Eq)
type Rational = Ratio Integer

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
