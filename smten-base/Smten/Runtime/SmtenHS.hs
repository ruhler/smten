
module Smten.Runtime.SmtenHS (SmtenHS0(..), ite) where

import Smten.Runtime.Formula
import Smten.Runtime.ErrorString

class SmtenHS0 a where
    error0 :: ErrorString -> a
    ite0 :: BoolF -> a -> a -> a

ite :: (SmtenHS0 a) => BoolF -> a -> a -> a
ite TrueF a _ = a
ite FalseF _ b = b
ite (NotF x) a b = ite x b a
ite p a b = ite0 p a b
    
