
module Seri.Type.Typeof (Typeof(..)) where

import Seri.Lit
import Seri.Type.Type
import Seri.Type.Sugar
    
class Typeof a where
    -- | Return the seri type of the given object.
    -- Returns UnknownT if the object is not well typed.
    typeof :: a -> Type

instance Typeof Lit where
    typeof (IntegerL {}) = integerT
    typeof (CharL {}) = charT



