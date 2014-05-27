
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RebindableSyntax #-}
module Smten.GHC.Num (
    Num(..)
     ) where

-- Note: this module is hardwired in the smten plugin to generate code to
-- Smten.Compiled.GHC.Num instead of Smten.Compiled.Smten.GHC.Num

import GHC.Base
import GHC.Prim
import GHC.Classes
import GHC.Types
import Smten.Smten.Base
import Smten.Data.Num0
import Smten.GHC.Integer.Type

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

    fromInteger = int_fromInteger

instance Num Integer where
    (+) = plusInteger
    (-) = minusInteger
    (*) = timesInteger
    fromInteger x = x

    abs = absInteger
    signum = signumInteger
    
