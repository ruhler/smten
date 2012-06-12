
module Seri.Serif.Library where

import Seri.FrontEnd.Typed
import Seri.Lambda

-- Integer
instance SeriType Integer where
  seritype _ = ConT "Integer"

-- Bool
instance SeriType Bool where
  seritype _ = ConT "Bool"

_serifP_True :: Typed Exp Bool
_serifP_True = conE' "True"

_serifP_False :: Typed Exp Bool
_serifP_False = conE' "False"

-- Unit
instance SeriType () where
    seritype _ = ConT "()"

_serifP___oparen__cparen :: Typed Exp ()
_serifP___oparen__cparen = conE' "()"

-- Tuple
instance SeriType2 (,) where
    seritype2 _ = ConT "(,)"

_serifP___oparen__comma__cparen :: (SeriType a, SeriType b)
    => Typed Exp (a -> b -> (a, b))
_serifP___oparen__comma__cparen = conE' "(,)"

instance SeriType3 (,,) where
    seritype3 _ = ConT "(,,)"

_serifP___oparen__comma__comma__cparen :: (SeriType a, SeriType b, SeriType c)
    => Typed Exp (a -> b -> c -> (a, b, c))
_serifP___oparen__comma__comma__cparen = conE' "(,,)"

-- List
instance SeriType1 [] where
    seritype1 _ = ConT "[]"
     
_serifP___obracket__cbracket :: (SeriType a) => Typed Exp ([a])
_serifP___obracket__cbracket = conE' "[]"

_serifP___colon :: (SeriType a) => Typed Exp (a -> [a] -> [a])
_serifP___colon = conE' ":"

-- Char
instance SeriType Char where
    seritype _ = ConT "Char"

-- Variable types
data VarT_a = VarT_a
instance SeriType VarT_a where
    seritype _ = VarT "a"

data VarT_b = VarT_b
instance SeriType VarT_b where
    seritype _ = VarT "b"

data VarT_m a = VarT_m
instance SeriType1 VarT_m where
    seritype1 _ = VarT "m"
