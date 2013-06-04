
-- | Built-in smten prelude declarations.
module Smten.Dec.Prelude (
    prelude, tupleD, listD, unitD,
    ) where

import Smten.Name
import Smten.Location
import Smten.Type
import Smten.Dec.Dec

ploc :: Location
ploc = Location "BuiltinPrelude" 0 0

tupleD :: Int -> Dec
tupleD i = 
  let nm = tupleN i
      vars = [TyVar (name ('x':show j)) StarK | j <- [1..i]]
  in DataD ploc nm vars [Con nm (map tyVarType vars)]

listD :: Dec
listD = DataD ploc listN [TyVar (name "a") StarK] [Con nilN [], Con consN [VarT (name "a") StarK, listT (VarT (name "a") StarK)]]

unitD :: Dec
unitD = DataD ploc unitN [] [Con unitN []]

prelude :: [Dec]
prelude = [
    DataD ploc arrowN [TyVar (name "a") StarK, TyVar (name "b") StarK] [],
    DataD ploc charN [] [],
    DataD ploc integerN [] [],
    unitD,
    tupleD 2, tupleD 3, tupleD 4,
    -- tuple 5, tuple 6, tuple 7, tuple 8, tuple 9,
    listD
    ]

