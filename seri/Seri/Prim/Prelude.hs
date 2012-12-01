
module Seri.Prim.Prelude (
    eq_IntegerP, eq_CharP,
    add_IntegerP, sub_IntegerP, mul_IntegerP,
    lt_IntegerP, leq_IntegerP, gt_IntegerP,
    show_IntegerP,
    preludePs,
    ) where

import Seri.Prim.Prim

preludePs :: [Prim]
preludePs = [
    eq_IntegerP, eq_CharP,
    add_IntegerP, sub_IntegerP, mul_IntegerP,
    lt_IntegerP, leq_IntegerP, gt_IntegerP,
    show_IntegerP
    ]

eq_IntegerP :: Prim
eq_IntegerP = binaryP "Prelude.__prim_eq_Integer" ((==) :: Integer -> Integer -> Bool)

eq_CharP :: Prim
eq_CharP = binaryP "Prelude.__prim_eq_Char" ((==) :: Char -> Char -> Bool)

add_IntegerP :: Prim
add_IntegerP = binaryP "Prelude.__prim_add_Integer" ((+) :: Integer -> Integer -> Integer)

sub_IntegerP :: Prim
sub_IntegerP = binaryP "Prelude.__prim_sub_Integer" ((-) :: Integer -> Integer -> Integer)

mul_IntegerP :: Prim
mul_IntegerP = binaryP "Prelude.__prim_mul_Integer" ((*) :: Integer -> Integer -> Integer)

lt_IntegerP :: Prim
lt_IntegerP = binaryP "Prelude.<" ((<) :: Integer -> Integer -> Bool)

leq_IntegerP :: Prim
leq_IntegerP = binaryP "Prelude.<=" ((<=) :: Integer -> Integer -> Bool)

gt_IntegerP :: Prim
gt_IntegerP = binaryP "Prelude.>" ((>) :: Integer -> Integer -> Bool)

show_IntegerP :: Prim
show_IntegerP = unaryP "Prelude.__prim_show_Integer" (show :: Integer -> String)

