
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fprof-auto-top #-}

module Smten.HaskellF.Lib.Prelude (
    HFArrow__,
    Char,
    Integer,
    IO,
    Unit__, __caseUnit__, __mkUnit__,
    Bool, __mkTrue, __mkFalse, __caseTrue, __caseFalse,
    Tuple2__, __mkTuple2__, __caseTuple2__,
    Tuple3__, __mkTuple3__, __caseTuple3__,
    Tuple4__, __mkTuple4__, __caseTuple4__,
    List__, __mkCons__, __mkNil__, __caseCons__, __caseNil__,
    String,
    Maybe, __mkJust, __mkNothing, __caseJust, __caseNothing,

    __prim_toInteger_Char, __prim_fromInteger_Char,
    __prim_eq_Integer,
    __prim_add_Integer, __prim_sub_Integer, __prim_mul_Integer,
    __prim_lt_Integer, __prim_gt_Integer, __prim_leq_Integer, __prim_geq_Integer,
    __prim_show_Integer,

    return_io, bind_io, putChar, getContents,
    error, valueof, numeric, __main_wrapper,
    ) where

import Prelude hiding (
    Char, String, Integer, Bool(..), IO, Maybe(..),
    error, putChar, getContents,
    )
import qualified Prelude as P
import qualified Prelude as Prelude

import Smten.Type
import Smten.Name
import Smten.Dec
import Smten.ExpH
import Smten.Prim
import Smten.Ppr
import qualified Smten.Type
import qualified Smten.ExpH
import Smten.HaskellF.HaskellF
import Smten.HaskellF.TH

type HFArrow__ = (->)

newtype Char = Char__s ExpH

instance SmtenT Char where
    smtenT _ = charT

instance HaskellF Char where
    box = Char__s
    unbox (Char__s v) = v

newtype Integer = Integer__s ExpH

instance SmtenT Integer where
    smtenT _ = integerT

instance HaskellF Integer where
    box = Integer__s
    unbox (Integer__s v) = v

instance Prelude.Num Integer where
    fromInteger = smtenHF
    (+) = P.error $ "+ for haskellf Integer not defined"
    (*) = P.error $ "* for haskellf Integer not defined"
    abs = P.error $ "abs for haskellf Integer not defined"
    signum = P.error $ "signum for haskellf Integer not defined"

newtype IO a = IO ExpH

instance SmtenT1 IO where
    smtenT1 _ = ConT ioN (ArrowK StarK StarK)

instance HaskellF1 IO where
    box1 = IO
    unbox1 (IO x) = x


id $
  let DataD _ n tyv cns = unitD
  in haskellf_Data n tyv cns

haskellf_Data boolN [] [Con trueN [], Con falseN []]

haskellf_Data maybeN [TyVar (name "a") StarK] [
    Con nothingN [],
    Con justN [VarT (name "a") StarK]
   ]

id $
  let DataD _ n tyv cns = tupleD 2
  in haskellf_Data n tyv cns

id $
  let DataD _ n tyv cns = tupleD 3
  in haskellf_Data n tyv cns

id $
  let DataD _ n tyv cns = tupleD 4
  in haskellf_Data n tyv cns

id $
  let DataD _ n tyv cns = listD
  in haskellf_Data n tyv cns

type String = List__ Char

__prim_toInteger_Char :: Function Char Integer
__prim_toInteger_Char = primHF toInteger_CharP

__prim_fromInteger_Char :: Function Integer Char
__prim_fromInteger_Char = primHF fromInteger_CharP

__prim_add_Integer :: Function Integer (Function Integer Integer)
__prim_add_Integer = primHF add_IntegerP

__prim_sub_Integer :: Function Integer (Function Integer Integer)
__prim_sub_Integer = primHF sub_IntegerP

__prim_mul_Integer :: Function Integer (Function Integer Integer)
__prim_mul_Integer = primHF mul_IntegerP

__prim_eq_Integer :: Function Integer (Function Integer Bool)
__prim_eq_Integer = primHF eq_IntegerP

__prim_lt_Integer :: Function Integer (Function Integer Bool)
__prim_lt_Integer = primHF lt_IntegerP

__prim_leq_Integer :: Function Integer (Function Integer Bool)
__prim_leq_Integer = primHF leq_IntegerP

__prim_geq_Integer :: Function Integer (Function Integer Bool)
__prim_geq_Integer = primHF geq_IntegerP

__prim_gt_Integer :: Function Integer (Function Integer Bool)
__prim_gt_Integer = primHF gt_IntegerP

__prim_show_Integer :: Function Integer String
__prim_show_Integer = primHF show_IntegerP

return_io :: (HaskellF a) => Function a (IO a)
return_io = primHF return_IOP

bind_io :: (HaskellF a, HaskellF b) => Function (IO a) (Function (Function a (IO b)) (IO b))
bind_io = primHF bind_IOP

putChar :: Function Char (IO Unit__)
putChar = primHF putCharP

getContents :: IO String
getContents = primHF getContentsP

error :: (HaskellF a) => Function String a
error = primHF errorP

numeric :: (HaskellF a) => a
numeric = primHF numericP 

valueof :: (HaskellF a) => Function a Integer
valueof = primHF valueofP

__main_wrapper :: IO Unit__ -> P.IO ()
__main_wrapper m
  | P.Just x <- de_ioEH (unbox m) = x >> return ()
  | otherwise = P.error $ "__main_wrapper: " ++ pretty (unbox m)

