
-- | Built-in smten prelude declarations.
module Smten.Dec.Prelude (
    prelude
    ) where

import Smten.Name
import Smten.Type
import Smten.Dec.Dec

tuple :: Int -> Dec
tuple i = 
  let nm = name $ "(" ++ replicate (i-1) ',' ++ ")"
      vars = [TyVar (name ('x':show j)) StarK | j <- [1..i]]
  in DataD nm vars [Con nm (map tyVarType vars)]

prelude :: [Dec]
prelude = [
    DataD (name "Char") [] [],
    DataD (name "Integer") [] [],
    DataD (name "()") [] [Con (name "()") []],
    tuple 2, tuple 3, tuple 4,
    -- tuple 5, tuple 6, tuple 7, tuple 8, tuple 9,
    DataD (name "[]") [TyVar (name "a") StarK] [Con (name "[]") [], Con (name ":") [VarT (name "a") StarK, listT (VarT (name "a") StarK)]]
    ]

