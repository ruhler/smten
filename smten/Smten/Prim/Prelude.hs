
module Smten.Prim.Prelude (
    preludePs,
    errorP,
    eq_IntegerP,
    add_IntegerP, sub_IntegerP, mul_IntegerP,
    lt_IntegerP, leq_IntegerP, gt_IntegerP, geq_IntegerP,
    toInteger_CharP, fromInteger_CharP,
    show_IntegerP,
    return_IOP, bind_IOP,
    putCharP, getContentsP,
    numericP, valueofP,
    traceP, traceEP
    ) where

import Debug.Trace

import Smten.Type
import Smten.Name
import Smten.Sig
import Smten.ExpH
import Smten.Exp
import Smten.Ppr
import Smten.Prim.Prim

preludePs :: [Prim]
preludePs = [
    errorP,
    eq_IntegerP,
    add_IntegerP, sub_IntegerP, mul_IntegerP,
    lt_IntegerP, leq_IntegerP,
    gt_IntegerP, geq_IntegerP,
    toInteger_CharP, fromInteger_CharP,
    show_IntegerP,
    return_IOP, bind_IOP,
    putCharP, getContentsP,
    numericP, valueofP,
    traceP, traceEP
    ]

errorP :: Prim
errorP = unaryTP "Prelude.error" errorEH

eq_IntegerP :: Prim
eq_IntegerP = binaryP "Prelude.__prim_eq_Integer"
  ((==) :: Integer -> Integer -> Bool)

toInteger_CharP :: Prim
toInteger_CharP = unaryP "Prelude.__prim_toInteger_Char"
  (toInteger . fromEnum :: Char -> Integer)

fromInteger_CharP :: Prim
fromInteger_CharP = unaryP "Prelude.__prim_fromInteger_Char"
  (toEnum . fromInteger :: Integer -> Char)

add_IntegerP :: Prim
add_IntegerP = binaryP "Prelude.__prim_add_Integer"
  ((+) :: Integer -> Integer -> Integer)

sub_IntegerP :: Prim
sub_IntegerP = binaryP "Prelude.__prim_sub_Integer"
  ((-) :: Integer -> Integer -> Integer)

mul_IntegerP :: Prim
mul_IntegerP = binaryP "Prelude.__prim_mul_Integer"
  ((*) :: Integer -> Integer -> Integer)

lt_IntegerP :: Prim
lt_IntegerP = binaryP "Prelude.__prim_lt_Integer"
  ((<) :: Integer -> Integer -> Bool)

leq_IntegerP :: Prim
leq_IntegerP = binaryP "Prelude.__prim_leq_Integer"
  ((<=) :: Integer -> Integer -> Bool)

gt_IntegerP :: Prim
gt_IntegerP = binaryP "Prelude.__prim_gt_Integer"
  ((>) :: Integer -> Integer -> Bool)

geq_IntegerP :: Prim
geq_IntegerP = binaryP "Prelude.__prim_geq_Integer"
  ((>=) :: Integer -> Integer -> Bool)

show_IntegerP :: Prim
show_IntegerP = unaryP "Prelude.__prim_show_Integer" (show :: Integer -> String)

return_IOP :: Prim
return_IOP = unaryP "Prelude.return_io" (return :: ExpH -> IO ExpH)

bind_IOP :: Prim
bind_IOP = binaryP "Prelude.bind_io" ((>>=) :: IO ExpH -> (ExpH -> IO ExpH) -> IO ExpH)

putCharP :: Prim
putCharP = unaryP "Prelude.putChar" putChar

getContentsP :: Prim
getContentsP = nullaryP "Prelude.getContents" getContents

numericP :: Prim
numericP =
  let f :: Type -> ExpH
      f nt = conEH (Sig (name "#" `nappend` name (show (nteval nt))) nt)
  in nullaryTP "Prelude.numeric" f

valueofP :: Prim
valueofP = 
  let f :: ExpH -> ExpH
      f x = integerEH (nteval (typeof (force x)))
  in unaryP "Prelude.valueof" f

traceP :: Prim
traceP = binaryP "Debug.Trace.trace" (trace :: String -> ExpH -> ExpH)

traceEP :: Prim
traceEP =
 let f :: ExpH -> ExpH -> ExpH
     f x a = trace (pretty (fromExpH x)) a
 in binaryP "Debug.Trace.traceE" f
        

