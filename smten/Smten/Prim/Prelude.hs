
module Smten.Prim.Prelude where

import Debug.Trace

import Smten.Type
import Smten.Name
import Smten.ExpH
import Smten.Ppr
import Smten.Prim.Prim

errorP :: Prim
errorP = unaryTP "Prelude.error" errorEH

__prim_eq_IntegerP :: Prim
__prim_eq_IntegerP = {-# SCC "__prim_eq_IntegerP" #-} binaryP "Prelude.__prim_eq_Integer"
  ((==) :: Integer -> Integer -> Bool)

__prim_toInteger_CharP :: Prim
__prim_toInteger_CharP = {-# SCC "__prim_toInteger_CharP" #-} unaryP "Prelude.__prim_toInteger_Char"
  (toInteger . fromEnum :: Char -> Integer)

__prim_fromInteger_CharP :: Prim
__prim_fromInteger_CharP = {-# SCC "__prim_fromInteger_CharP" #-} unaryP "Prelude.__prim_fromInteger_Char"
  (toEnum . fromInteger :: Integer -> Char)

__prim_add_IntegerP :: Prim
__prim_add_IntegerP = {-# SCC "__prim_add_IntegerP" #-} binaryP "Prelude.__prim_add_Integer"
  ((+) :: Integer -> Integer -> Integer)

__prim_sub_IntegerP :: Prim
__prim_sub_IntegerP = {-# SCC "__prim_sub_IntegerP" #-} binaryP "Prelude.__prim_sub_Integer"
  ((-) :: Integer -> Integer -> Integer)

__prim_mul_IntegerP :: Prim
__prim_mul_IntegerP = {-# SCC "__prim_mul_IntegerP" #-} binaryP "Prelude.__prim_mul_Integer"
  ((*) :: Integer -> Integer -> Integer)

__prim_lt_IntegerP :: Prim
__prim_lt_IntegerP = {-# SCC "__prim_lt_IntegerP" #-} binaryP "Prelude.__prim_lt_Integer"
  ((<) :: Integer -> Integer -> Bool)

__prim_leq_IntegerP :: Prim
__prim_leq_IntegerP = {-# SCC "__prim_leq_IntegerP" #-} binaryP "Prelude.__prim_leq_Integer"
  ((<=) :: Integer -> Integer -> Bool)

__prim_gt_IntegerP :: Prim
__prim_gt_IntegerP = {-# SCC "__prim_gt_IntegerP" #-} binaryP "Prelude.__prim_gt_Integer"
  ((>) :: Integer -> Integer -> Bool)

__prim_geq_IntegerP :: Prim
__prim_geq_IntegerP = {-# SCC "__prim_geq_IntegerP" #-} binaryP "Prelude.__prim_geq_Integer"
  ((>=) :: Integer -> Integer -> Bool)

__prim_show_IntegerP :: Prim
__prim_show_IntegerP = {-# SCC "__prim_show_IntegerP" #-} unaryP "Prelude.__prim_show_Integer" (show :: Integer -> String)

return_ioP :: Prim
return_ioP = unaryP "Prelude.return_io" (return :: ExpH -> IO ExpH)

bind_ioP :: Prim
bind_ioP = binaryP "Prelude.bind_io" ((>>=) :: IO ExpH -> (ExpH -> IO ExpH) -> IO ExpH)

putCharP :: Prim
putCharP = {-# SCC "putCharP" #-} unaryP "Prelude.putChar" putChar

getContentsP :: Prim
getContentsP = {-# SCC "getContentsP" #-} nullaryP "Prelude.getContents" getContents

numericP :: Prim
numericP = {-# SCC "numericP" #-}
  let f :: Type -> ExpH
      f nt = conEH nt (name "#" `nappend` name (show (nteval nt))) []
  in nullaryTP "Prelude.numeric" f

valueofP :: Prim
valueofP = {-# SCC "valueofP" #-}
  let f :: ExpH -> ExpH
      f x = integerEH (nteval (typeof x))
  in unaryP "Prelude.valueof" f

traceP :: Prim
traceP = binaryP "Debug.Trace.trace" (trace :: String -> ExpH -> ExpH)

traceEP :: Prim
traceEP =
 let f :: ExpH -> ExpH -> ExpH
     f x a = trace (pretty (fromExpH x)) a
 in binaryP "Debug.Trace.traceE" f
        

