
-- | Built-in seri prelude declarations.
module Seri.Dec.Prelude (
    prelude
    ) where

import Seri.Name
import Seri.Type
import Seri.Dec.Dec

tuple :: Int -> Dec
tuple i = 
  let nm = name $ "(" ++ replicate (i-1) ',' ++ ")"
      vars = [NormalTV (name [c]) | c <- take i "abcdefghijklmnopqrstuvwxyz"]
  in DataD nm vars [Con nm (map tyVarType vars)]

prelude :: [Dec]
prelude = [
    DataD (name "Char") [] [],
    DataD (name "Integer") [] [],
    DataD (name "()") [] [Con (name "()") []],
    tuple 2, tuple 3, tuple 4, tuple 5, tuple 6, tuple 7, tuple 8, tuple 9,
    DataD (name "[]") [NormalTV (name "a")] [Con (name "[]") [], Con (name ":") [VarT (name "a"), listT (VarT (name "a"))]]
    ]

