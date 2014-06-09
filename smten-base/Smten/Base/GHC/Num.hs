
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RebindableSyntax #-}
module Smten.Base.GHC.Num (
    Num(..)
     ) where

import GHC.Base
import GHC.Integer

infixl 7 *
infixl 6 +, -

class Num a where
    (+), (-), (*) :: a -> a -> a
    negate :: a -> a
    abs :: a -> a
    signum :: a -> a
    fromInteger :: Integer -> a

    {-# INLINE (-) #-}
    {-# INLINE negate #-}
    x - y = x + negate y
    negate x = 0 - x

instance Num Int where
    I# x + I# y = I# (x +# y)
    I# x - I# y = I# (x -# y)
    negate (I# x) = I# (negateInt# x)
    I# x * I# y = I# (x *# y)
    abs n  = case n `geInt` 0 of
                True -> n 
                False -> negate n

    signum n | n `ltInt` 0 = negate 1
             | n `eqInt` 0 = 0
             | otherwise = 1

    fromInteger i = I# (integerToInt i)

instance Num Integer where
    (+) = plusInteger
    (-) = minusInteger
    (*) = timesInteger
    fromInteger x = x

    abs = absInteger
    signum = signumInteger
    
