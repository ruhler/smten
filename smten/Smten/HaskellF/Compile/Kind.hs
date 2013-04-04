
module Smten.HaskellF.Compile.Kind (knum) where

import Smten.Type

knum :: Kind -> Integer
knum StarK = 0
knum NumK = 0
knum (ArrowK a b) = 1 + knum a
knum (VarK i) = 0 -- default to StarK
knum UnknownK = 0

