
module Seri.Prim.Prelude (
    preludePs,
    errorP,
    eq_IntegerP, toInteger_CharP, fromInteger_CharP,
    add_IntegerP, sub_IntegerP, mul_IntegerP,
    lt_IntegerP, leq_IntegerP, gt_IntegerP, geq_IntegerP,
    show_IntegerP,
    return_IOP, bind_IOP, nobind_IOP, fail_IOP,
    putCharP, getContentsP,
    numericP, valueofP,
    traceP,
    ) where

import Debug.Trace

import Seri.Type
import Seri.Name
import Seri.Sig
import Seri.ExpH
import Seri.Prim.Prim

preludePs :: [Prim]
preludePs = [
    errorP,
    eq_IntegerP, toInteger_CharP, fromInteger_CharP,
    add_IntegerP, sub_IntegerP, mul_IntegerP,
    lt_IntegerP, leq_IntegerP, gt_IntegerP, geq_IntegerP,
    show_IntegerP,
    return_IOP,  fail_IOP,
    bind_IOP, nobind_IOP,
    putCharP, getContentsP,
    numericP, valueofP,
    traceP
    ]

errorP :: Prim
errorP = unaryTP "Prelude.error" errorEH

eq_IntegerP :: Prim
eq_IntegerP = binaryP "Prelude.__prim_eq_Integer" ((==) :: Integer -> Integer -> Bool)

toInteger_CharP :: Prim
toInteger_CharP =
 let f :: Char -> Integer
     f = toInteger . fromEnum
 in unaryP "Prelude.__prim_toInteger_Char" f

fromInteger_CharP :: Prim
fromInteger_CharP =
 let f :: Integer -> Char
     f = toEnum . fromInteger
 in unaryP "Prelude.__prim_fromInteger_Char" f

add_IntegerP :: Prim
add_IntegerP = binaryP "Prelude.__prim_add_Integer" ((+) :: Integer -> Integer -> Integer)

sub_IntegerP :: Prim
sub_IntegerP = binaryP "Prelude.__prim_sub_Integer" ((-) :: Integer -> Integer -> Integer)

mul_IntegerP :: Prim
mul_IntegerP = binaryP "Prelude.__prim_mul_Integer" ((*) :: Integer -> Integer -> Integer)

lt_IntegerP :: Prim
lt_IntegerP = binaryP "Prelude.__prim_lt_Integer" ((<) :: Integer -> Integer -> Bool)

leq_IntegerP :: Prim
leq_IntegerP = binaryP "Prelude.__prim_leq_Integer" ((<=) :: Integer -> Integer -> Bool)

gt_IntegerP :: Prim
gt_IntegerP = binaryP "Prelude.__prim_gt_Integer" ((>) :: Integer -> Integer -> Bool)

geq_IntegerP :: Prim
geq_IntegerP = binaryP "Prelude.__prim_geq_Integer" ((>=) :: Integer -> Integer -> Bool)

show_IntegerP :: Prim
show_IntegerP = unaryP "Prelude.__prim_show_Integer" (show :: Integer -> String)

return_IOP :: Prim
return_IOP = unaryP "Prelude.return_io" (return :: ExpH -> IO ExpH)

bind_IOP :: Prim
bind_IOP = binaryP "Prelude.bind_io" ((>>=) :: IO ExpH -> (ExpH -> IO ExpH) -> IO ExpH)

nobind_IOP :: Prim
nobind_IOP = binaryP "Prelude.nobind_io" ((>>) :: IO ExpH -> IO ExpH -> IO ExpH)

fail_IOP :: Prim
fail_IOP = unaryP "Prelude.fail_io" (fail :: String -> IO ExpH)

putCharP :: Prim
putCharP = unaryP "Prelude.putChar" putChar

getContentsP :: Prim
getContentsP = nullaryP "Prelude.getContents" getContents

numericP :: Prim
numericP =
  let f :: Type -> ExpH
      f (NumT nt) = conEH (Sig (name "#" `nappend` name (show (nteval nt))) (NumT nt))
  in nullaryTP "Prelude.numeric" f

valueofP :: Prim
valueofP = 
  let f :: ExpH -> ExpH
      f x =
        let NumT nt = typeof x
        in integerEH (nteval nt)
  in unaryP "Prelude.valueof" f

traceP :: Prim
traceP = binaryP "Debug.Trace.trace" (trace :: String -> ExpH -> ExpH)
        

