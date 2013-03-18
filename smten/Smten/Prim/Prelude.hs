
module Smten.Prim.Prelude (
    preludePs,
    errorP,
    eq_IntegerP,
    add_IntegerP, sub_IntegerP, mul_IntegerP,
    lt_IntegerP, leq_IntegerP, gt_IntegerP, geq_IntegerP,
    toInteger_CharP, fromInteger_CharP,
    show_IntegerP,
    return_IOP, bind_IOP, nobind_IOP, fail_IOP,
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
    p_prim eq_IntegerP,
    p_prim add_IntegerP, p_prim sub_IntegerP, p_prim mul_IntegerP,
    p_prim lt_IntegerP, p_prim leq_IntegerP,
    p_prim gt_IntegerP, p_prim geq_IntegerP,
    p_prim toInteger_CharP, p_prim fromInteger_CharP,
    show_IntegerP,
    return_IOP,  fail_IOP,
    bind_IOP, nobind_IOP,
    putCharP, getContentsP,
    numericP, valueofP,
    traceP, traceEP
    ]

errorP :: Prim
errorP = unaryTP "Prelude.error" errorEH

eq_IntegerP :: PrimF (Integer -> Integer -> Bool)
eq_IntegerP = binaryPF "Prelude.__prim_eq_Integer" (==)

toInteger_CharP :: PrimF (Char -> Integer)
toInteger_CharP = unaryPF "Prelude.__prim_toInteger_Char" $
    toInteger . fromEnum

fromInteger_CharP :: PrimF (Integer -> Char)
fromInteger_CharP = unaryPF "Prelude.__prim_fromInteger_Char" $
    toEnum . fromInteger

add_IntegerP :: PrimF (Integer -> Integer -> Integer)
add_IntegerP = binaryPF "Prelude.__prim_add_Integer" (+)

sub_IntegerP :: PrimF (Integer -> Integer -> Integer)
sub_IntegerP = binaryPF "Prelude.__prim_sub_Integer" (-)

mul_IntegerP :: PrimF (Integer -> Integer -> Integer)
mul_IntegerP = binaryPF "Prelude.__prim_mul_Integer" (*)

lt_IntegerP :: PrimF (Integer -> Integer -> Bool)
lt_IntegerP = binaryPF "Prelude.__prim_lt_Integer" (<)

leq_IntegerP :: PrimF (Integer -> Integer -> Bool)
leq_IntegerP = binaryPF "Prelude.__prim_leq_Integer" (<=)

gt_IntegerP :: PrimF (Integer -> Integer -> Bool)
gt_IntegerP = binaryPF "Prelude.__prim_gt_Integer" (>)

geq_IntegerP :: PrimF (Integer -> Integer -> Bool)
geq_IntegerP = binaryPF "Prelude.__prim_geq_Integer" (>=)

show_IntegerP :: Prim
show_IntegerP = unaryP "Prelude.__prim_show_Integer" (show :: Integer -> String)

return_IOP :: Prim
return_IOP = unaryP "Prelude.return_io" (return :: Thunk -> IO Thunk)

bind_IOP :: Prim
bind_IOP = binaryP "Prelude.bind_io" ((>>=) :: IO Thunk -> (Thunk -> IO Thunk) -> IO Thunk)

nobind_IOP :: Prim
nobind_IOP = binaryP "Prelude.nobind_io" ((>>) :: IO Thunk -> IO Thunk -> IO Thunk)

fail_IOP :: Prim
fail_IOP = unaryP "Prelude.fail_io" (fail :: String -> IO Thunk)

putCharP :: Prim
putCharP = unaryP "Prelude.putChar" putChar

getContentsP :: Prim
getContentsP = nullaryP "Prelude.getContents" getContents

numericP :: Prim
numericP =
  let f :: Type -> Thunk
      f nt = conEH (Sig (name "#" `nappend` name (show (nteval nt))) nt)
  in nullaryTP "Prelude.numeric" f

valueofP :: Prim
valueofP = 
  let f :: Thunk -> Thunk
      f x = integerEH (nteval (typeof x))
  in unaryP "Prelude.valueof" f

traceP :: Prim
traceP = binaryP "Debug.Trace.trace" (trace :: String -> Thunk -> Thunk)

traceEP :: Prim
traceEP =
 let f :: Thunk -> Thunk -> Thunk
     f x a = trace (pretty (fromExpH x)) a
 in binaryP "Debug.Trace.traceE" f
        

