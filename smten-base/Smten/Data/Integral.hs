
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Integral (
    Integral(..),
    ) where

import GHC.Classes
import GHC.Enum
import Smten.GHC.Integer.Type
import Smten.Smten.Integer
import GHC.Num (Num(..))
import GHC.Types
import Smten.Data.Integral0

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
    quotRem a b = (quot a b, rem a b)
    quot = int_quot
    rem = int_rem

