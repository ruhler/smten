
module Seri.Lambda.Prelude (prelude) where

import Seri.Lambda.IR
import Seri.Lambda.Types

tuple :: Int -> Dec
tuple i = 
  let name = "(" ++ replicate (i-1) ',' ++ ")"
      vars = [[c] | c <- take i "abcdefghijklmnopqrstuvwxyz"]
  in DataD name vars [Con name (map VarT vars)]

prelude :: [Dec]
prelude = [
    PrimD (TopSig "undefined" [] (VarT "a")),
    DataD "Char" [] [],
    DataD "Integer" [] [],
    DataD "()" [] [Con "()" []],
    tuple 2, tuple 3, tuple 4,
    DataD "[]" ["a"] [Con "[]" [], Con ":" [VarT "a", listT (VarT "a")]]
    ]

