
{-# LANGUAGE PatternGuards #-}

module Seri.Type.Typeof (Typeof(..)) where

import Seri.Bit
import Seri.Lit
import Seri.Type.Type
import Seri.Type.Sugar
    
class Typeof a where
    -- | Return the seri type of the given object.
    -- Returns UnknownT if the object is not well typed.
    typeof :: a -> Type

instance Typeof Lit where
    typeof l
      | Just _ <- de_integerL l = integerT
      | Just _ <- de_charL l = charT
      | Just v <- de_bitL l = bitT (bv_width v)
      | otherwise = UnknownT


