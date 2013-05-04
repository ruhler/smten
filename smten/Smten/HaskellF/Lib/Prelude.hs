
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fprof-auto-top #-}

module Smten.HaskellF.Lib.Prelude (
    Char,
    Integer,
    IO,
    Unit__(Unit__), __caseUnit__,
    Bool(True, False), __caseTrue, __caseFalse,
    Tuple2__(Tuple2__), __caseTuple2__,
    Tuple3__(Tuple3__), __caseTuple3__,
    Tuple4__(Tuple4__), __caseTuple4__,
    List__(Cons__, Nil__), __caseCons__, __caseNil__, de_listHF,
    String,
    Maybe(Just, Nothing), __caseJust, __caseNothing,

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

data Char =
     Char P.Char
   | Char__s ExpH

instance SmtenT Char where
    smtenT _ = charT

instance HaskellF Char where
    box e
      | P.Just v <- de_charEH e = Char v
      | otherwise = Char__s e

    unbox x
      | Char v <- x = charEH v
      | Char__s v <- x = v

instance SmtenHF P.Char Char where
    smtenHF = Char
    de_smtenHF (Char v) = P.Just v
    de_smtenHF (Char__s v) = de_smtenEH v

    
data Integer =
        Integer !P.Integer
      | Integer__s ExpH

instance SmtenT Integer where
    smtenT _ = integerT

instance HaskellF Integer where
    box e
     | P.Just v <- de_integerEH e = Integer v
     | otherwise = Integer__s e

    unbox x
     | Integer v <- x = integerEH v
     | Integer__s v <- x = v

instance SmtenHF P.Integer Integer where  
    smtenHF = Integer

    de_smtenHF (Integer x) = P.Just x
    de_smtenHF (Integer__s v) = de_smtenEH v

instance Prelude.Num Integer where
    fromInteger = Integer
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
derive_SmtenHF ''P.Bool ''Bool

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

instance (SmtenHF ca fa, SmtenHF cb fb) => SmtenHF (ca, cb) (Tuple2__ fa fb) where
    smtenHF (a, b) = Tuple2__ (smtenHF a) (smtenHF b)
    de_smtenHF (Tuple2__ a b) = do
        a' <- de_smtenHF a
        b' <- de_smtenHF b    
        return (a', b')
    de_smtenHF (Tuple2____s v) = de_smtenEH v

id $
  let DataD _ n tyv cns = listD
  in haskellf_Data n tyv cns

instance (SmtenHF c f) => SmtenHF [c] (List__ f) where
    smtenHF [] = Nil__
    smtenHF (x:xs) = Cons__ (smtenHF x) (smtenHF xs)
    
    de_smtenHF Nil__ = P.Just []
    de_smtenHF (Cons__ x xs) = do
        x' <- de_smtenHF x
        xs' <- de_smtenHF xs
        return (x':xs')
    de_smtenHF (List____s v) = de_smtenEH v

de_listHF :: List__ a -> P.Maybe [a]
de_listHF Nil__ = P.Just []
de_listHF (Cons__ x xs) = do
    xs' <- de_listHF xs
    return (x:xs')
de_listHF (List____s v) = P.Nothing

type String = List__ Char

__prim_toInteger_Char :: Char -> Integer
__prim_toInteger_Char = unaryHF toInteger_CharP

__prim_fromInteger_Char :: Integer -> Char
__prim_fromInteger_Char = unaryHF fromInteger_CharP

__prim_add_Integer :: Integer -> Integer -> Integer
__prim_add_Integer (Integer av) (Integer bv) = smtenHF (p_impl add_IntegerP av bv)
__prim_add_Integer a b = primHF (p_prim add_IntegerP) a b

__prim_sub_Integer :: Integer -> Integer -> Integer
__prim_sub_Integer (Integer av) (Integer bv) = smtenHF (p_impl sub_IntegerP av bv)
__prim_sub_Integer a b = primHF (p_prim sub_IntegerP) a b

__prim_mul_Integer :: Integer -> Integer -> Integer
__prim_mul_Integer (Integer av) (Integer bv) = smtenHF (p_impl mul_IntegerP av bv)
__prim_mul_Integer a b = primHF (p_prim mul_IntegerP) a b

__prim_eq_Integer :: Integer -> Integer -> Bool
__prim_eq_Integer (Integer av) (Integer bv) = smtenHF (p_impl eq_IntegerP av bv)
__prim_eq_Integer a b = primHF (p_prim eq_IntegerP) a b

__prim_lt_Integer :: Integer -> Integer -> Bool
__prim_lt_Integer (Integer av) (Integer bv) = smtenHF (p_impl lt_IntegerP av bv)
__prim_lt_Integer a b = primHF (p_prim lt_IntegerP) a b

__prim_leq_Integer :: Integer -> Integer -> Bool
__prim_leq_Integer (Integer av) (Integer bv) = smtenHF (p_impl leq_IntegerP av bv)
__prim_leq_Integer a b = primHF (p_prim leq_IntegerP) a b

__prim_geq_Integer :: Integer -> Integer -> Bool
__prim_geq_Integer (Integer av) (Integer bv) = smtenHF (p_impl geq_IntegerP av bv)
__prim_geq_Integer a b = primHF (p_prim geq_IntegerP) a b

__prim_gt_Integer :: Integer -> Integer -> Bool
__prim_gt_Integer (Integer av) (Integer bv) = smtenHF (p_impl gt_IntegerP av bv)
__prim_gt_Integer a b = primHF (p_prim gt_IntegerP) a b

__prim_show_Integer :: Integer -> String
__prim_show_Integer = primHF show_IntegerP

return_io :: (HaskellF a) => a -> IO a
return_io = primHF return_IOP

bind_io :: (HaskellF a, HaskellF b) => IO a -> (a -> IO b) -> IO b
bind_io = primHF bind_IOP

putChar :: Char -> IO Unit__
putChar = primHF putCharP

getContents :: IO String
getContents = primHF getContentsP

error :: (HaskellF a) => String -> a
error = primHF errorP

numeric :: (HaskellF a) => a
numeric = primHF numericP 

valueof :: (HaskellF a) => a -> Integer
valueof = primHF valueofP

__main_wrapper :: IO Unit__ -> P.IO ()
__main_wrapper m
  | P.Just x <- de_ioEH (unbox m) = x >> return ()
  | otherwise = P.error $ "__main_wrapper: " ++ pretty (unbox m)

