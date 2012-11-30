
module Seri.Prim.Prelude (
    eq_IntegerP, notP,
    preludePs,
    ) where

import Seri.Prim.Prim

eq_IntegerP :: Prim
eq_IntegerP = binaryP "Prelude.__prim_eq_Integer" ((==) :: Integer -> Integer -> Bool)

notP :: Prim
notP = unaryP "Prelude.not" not

preludePs :: [Prim]
preludePs = [eq_IntegerP, notP]

