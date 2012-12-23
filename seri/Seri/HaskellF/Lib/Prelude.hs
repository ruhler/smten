
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Seri.HaskellF.Lib.Prelude (
    Char,
    Integer,
    Bit,
    IO,
    Unit__(Unit__), __caseUnit__,
    Bool(True, False), __caseTrue, __caseFalse,
    List__(Cons__, Nil__), __caseCons__, __caseNil__,

    N__0, N__2p1, N__2p0, N__PLUS, N__MINUS, N__TIMES,

    __prim_toInteger_Char,
    __prim_eq_Integer,
    __prim_add_Integer, __prim_sub_Integer, __prim_mul_Integer,
    __prim_lt_Integer, __prim_gt_Integer, __prim_leq_Integer, __prim_geq_Integer,
    __prim_show_Integer,
    return_io, bind_io, nobind_io, fail_io, putChar, getContents,

    __prim_eq_Bit, __prim_show_Bit,
    __prim_lt_Bit, __prim_gt_Bit, __prim_leq_Bit, __prim_geq_Bit,
    __prim_add_Bit, __prim_sub_Bit, __prim_mul_Bit,
    __prim_fromInteger_Bit,
    __prim_shl_Bit, __prim_lshr_Bit,
    __prim_or_Bit, __prim_and_Bit, __prim_not_Bit,
    __prim_zeroExtend_Bit,
    __prim_truncate_Bit, __prim_concat_Bit, __prim_extract_Bit,
    error,
    __main_wrapper,
    valueof, numeric, trace,
    ) where

import Prelude hiding (
    Char, String, Integer, Bool(..), IO,
    error, putChar, getContents,
    )
import qualified Prelude as P

import Seri.Type
import Seri.Name
import Seri.ExpH
import Seri.Prim
import Seri.Ppr
import Seri.HaskellF.Symbolic

data Char =
     Char P.Char
   | Char__s ExpH

instance SeriT Char where
    seriT _ = charT

instance Symbolic Char where
    box e
      | Just v <- de_charEH e = Char v
      | otherwise = Char__s e

    unbox x
      | Char v <- x = charEH v
      | Char__s v <- x = v

instance SeriS P.Char Char where
    seriS = Char
    de_seriS (Char v) = Just v
    de_seriS (Char__s v) = de_seriEH v

    
data Integer =
        Integer P.Integer
      | Integer__s ExpH

instance SeriT Integer where
    seriT _ = integerT

instance Symbolic Integer where
    box e
     | Just v <- de_integerEH e = Integer v
     | otherwise = Integer__s e

    unbox x
     | Integer v <- x = integerEH v
     | Integer__s v <- x = v

instance SeriS P.Integer Integer where  
    seriS = Integer

    de_seriS (Integer x) = Just x
    de_seriS (Integer__s v) = de_seriEH v

instance Prelude.Num Integer where
    fromInteger = Integer
    (+) = P.error $ "+ for haskellf Integer not defined"
    (*) = P.error $ "* for haskellf Integer not defined"
    abs = P.error $ "abs for haskellf Integer not defined"
    signum = P.error $ "signum for haskellf Integer not defined"

newtype Bit n = Bit ExpH

instance SeriT1 Bit where
    seriT1 _ = ConT (name "Bit")

instance Symbolic1 Bit where
    box1 = Bit
    unbox1 (Bit x) = x

newtype IO a = IO ExpH

instance SeriT1 IO where
    seriT1 _ = ConT (name "IO")

instance Symbolic1 IO where
    box1 = IO
    unbox1 (IO x) = x

data Unit__ = Unit__ | Unit__s ExpH

instance SeriT Unit__ where
    seriT _ = unitT
    
instance Symbolic Unit__ where
    box e 
      | Just [] <- de_conS "()" e = Unit__
      | otherwise = Unit__s e

    unbox x
      | Unit__ <- x = conS x "()" []
      | Unit__s v <- x = v

__caseUnit__ :: (Symbolic a) => Unit__ -> a -> a -> a
__caseUnit__ x y n
  | Unit__ <- x = y
  | Unit__s _ <- x = caseS "()" x y n
  | otherwise = n

data Bool =
    True
  | False
  | Bool_s ExpH

instance SeriT Bool where
    seriT _ = boolT

instance Symbolic Bool where
    box e
      | Just [] <- de_conS "True" e = True
      | Just [] <- de_conS "False" e = False
      | otherwise = Bool_s e

    unbox x
      | True <- x = conS x "True" []
      | False <- x = conS x "False" []
      | Bool_s v <- x = v

instance SeriS P.Bool Bool where
    seriS P.True = True
    seriS P.False = False

    de_seriS True = Just P.True
    de_seriS False = Just P.False
    de_seriS (Bool_s v) = de_seriEH v

__caseTrue :: (Symbolic a) => Bool -> a -> a -> a
__caseTrue x y n
  | True <- x = y
  | Bool_s _ <- x = caseS "True" x y n
  | otherwise = n

__caseFalse :: (Symbolic a) => Bool -> a -> a -> a
__caseFalse x y n
  | False <- x = y
  | Bool_s _ <- x = caseS "False" x y n
  | otherwise = n

data List__ a =
      Nil__ 
    | Cons__ a (List__ a)
    | List__s ExpH

instance SeriT1 List__ where
    seriT1 _ = seriT1 [()]

instance Symbolic1 List__ where
    box1 e
     | Just [] <- de_conS "[]" e = Nil__
     | Just [x, xs] <- de_conS ":" e = Cons__ (box x) (box xs)
     | otherwise = List__s e

    unbox1 x
     | Nil__ <- x = conS x "[]" []
     | Cons__ a b <- x = conS x ":" [unbox a, unbox b]
     | List__s v <- x = v

instance (SeriS c f) => SeriS [c] (List__ f) where
    seriS [] = Nil__
    seriS (x:xs) = Cons__ (seriS x) (seriS xs)
    
    de_seriS Nil__ = Just []
    de_seriS (Cons__ x xs) = do
        x' <- de_seriS x
        xs' <- de_seriS xs
        return (x':xs')
    de_seriS (List__s v) = de_seriEH v

__caseNil__ :: (Symbolic a, Symbolic z) => List__ a -> z -> z -> z
__caseNil__ x y n
  | Nil__ <- x = y
  | List__s _ <- x = caseS "[]" x y n
  | otherwise = n

__caseCons__ :: (Symbolic a, Symbolic z) => List__ a -> (a -> List__ a -> z) -> z -> z
__caseCons__ x y n
  | Cons__ a b <- x = y a b
  | List__s _ <- x = caseS ":" x y n
  | otherwise = n

type String = List__ Char

newtype N__0 = N__0 ExpH

instance SeriT N__0 where
    seriT _ = NumT (ConNT 0)

instance Symbolic N__0 where
    box = N__0
    unbox (N__0 x) = x

newtype N__1 = N__1 ExpH

instance SeriT N__1 where
    seriT _ = NumT (ConNT 1)

instance Symbolic N__1 where
    box = N__1
    unbox (N__1 x) = x

newtype N__2 = N__2 ExpH

instance SeriT N__2 where
    seriT _ = NumT (ConNT 2)

instance Symbolic N__2 where
    box = N__2
    unbox (N__2 x) = x

newtype N__PLUS a b = N__PLUS ExpH

instance SeriT2 N__PLUS where
    seriT2 _ = ConT (name "+")

instance Symbolic2 N__PLUS where
    box2 = N__PLUS
    unbox2 (N__PLUS x) = x

newtype N__MINUS a b = N__MINUS ExpH

instance SeriT2 N__MINUS where
    seriT2 _ = ConT (name "-")

instance Symbolic2 N__MINUS where
    box2 = N__MINUS
    unbox2 (N__MINUS x) = x

newtype N__TIMES a b = N__TIMES ExpH

instance SeriT2 N__TIMES where
    seriT2 _ = ConT (name "*")

instance Symbolic2 N__TIMES where
    box2 = N__TIMES
    unbox2 (N__TIMES x) = x

type N__2p0 a = N__TIMES N__2 a
type N__2p1 a = N__PLUS (N__2p0 a) N__1


__prim_toInteger_Char :: Char -> Integer
__prim_toInteger_Char =
    let f :: P.Char -> P.Integer 
        f = toInteger . fromEnum
    in unaryS toInteger_CharP f

__prim_eq_Integer :: Integer -> Integer -> Bool
__prim_eq_Integer = binaryS eq_IntegerP ((==) :: P.Integer -> P.Integer -> P.Bool)

__prim_add_Integer :: Integer -> Integer -> Integer
__prim_add_Integer = binaryS add_IntegerP ((+) :: P.Integer -> P.Integer -> P.Integer)

__prim_sub_Integer :: Integer -> Integer -> Integer
__prim_sub_Integer = binaryS sub_IntegerP ((-) :: P.Integer -> P.Integer -> P.Integer)

__prim_mul_Integer :: Integer -> Integer -> Integer
__prim_mul_Integer = binaryS mul_IntegerP ((*) :: P.Integer -> P.Integer -> P.Integer)

__prim_lt_Integer :: Integer -> Integer -> Bool
__prim_lt_Integer = binaryS lt_IntegerP ((<) :: P.Integer -> P.Integer -> P.Bool)

__prim_leq_Integer :: Integer -> Integer -> Bool
__prim_leq_Integer = binaryS leq_IntegerP ((<=) :: P.Integer -> P.Integer -> P.Bool)

__prim_gt_Integer :: Integer -> Integer -> Bool
__prim_gt_Integer = binaryS gt_IntegerP ((>) :: P.Integer -> P.Integer -> P.Bool)

__prim_geq_Integer :: Integer -> Integer -> Bool
__prim_geq_Integer = binaryS geq_IntegerP ((>=) :: P.Integer -> P.Integer -> P.Bool)

__prim_show_Integer :: Integer -> String
__prim_show_Integer = primS show_IntegerP

return_io :: (Symbolic a) => a -> IO a
return_io = primS return_IOP

bind_io :: (Symbolic a, Symbolic b) => IO a -> (a -> IO b) -> IO b
bind_io = primS bind_IOP

nobind_io :: (Symbolic a, Symbolic b) => IO a -> IO b -> IO b
nobind_io = primS nobind_IOP

fail_io :: (Symbolic a) => String -> IO a
fail_io = primS fail_IOP

putChar :: Char -> IO Unit__
putChar = primS putCharP

getContents :: IO String
getContents = primS getContentsP

__prim_eq_Bit :: (Symbolic n) => Bit n -> Bit n -> Bool
__prim_eq_Bit = primS eq_BitP

__prim_show_Bit :: (Symbolic n) => Bit n -> String
__prim_show_Bit = primS show_BitP

__prim_add_Bit :: (Symbolic n) => Bit n -> Bit n -> Bit n
__prim_add_Bit = primS add_BitP

__prim_sub_Bit :: (Symbolic n) => Bit n -> Bit n -> Bit n
__prim_sub_Bit = primS sub_BitP

__prim_mul_Bit :: (Symbolic n) => Bit n -> Bit n -> Bit n
__prim_mul_Bit = primS mul_BitP

__prim_fromInteger_Bit :: (Symbolic n) => Integer -> Bit n
__prim_fromInteger_Bit = primS fromInteger_BitP

__prim_shl_Bit :: (Symbolic n) => Bit n -> Bit n -> Bit n
__prim_shl_Bit = primS shl_BitP

__prim_lshr_Bit :: (Symbolic n) => Bit n -> Bit n -> Bit n
__prim_lshr_Bit = primS lshr_BitP

__prim_or_Bit :: (Symbolic n) => Bit n -> Bit n -> Bit n
__prim_or_Bit = primS or_BitP

__prim_and_Bit :: (Symbolic n) => Bit n -> Bit n -> Bit n
__prim_and_Bit = primS and_BitP

__prim_not_Bit :: (Symbolic n) => Bit n -> Bit n
__prim_not_Bit = primS not_BitP

__prim_zeroExtend_Bit :: (Symbolic n, Symbolic m) => Bit n -> Bit m
__prim_zeroExtend_Bit = primS zeroExtend_BitP

__prim_truncate_Bit :: (Symbolic n, Symbolic m) => Bit n -> Bit m
__prim_truncate_Bit = primS truncate_BitP

__prim_concat_Bit :: (Symbolic n, Symbolic m) => Bit n -> Bit m -> Bit (N__PLUS n m)
__prim_concat_Bit = primS concat_BitP

__prim_extract_Bit :: (Symbolic n, Symbolic m) => Bit n -> Integer -> Bit m
__prim_extract_Bit = primS extract_BitP

error :: (Symbolic a) => String -> a
error = primS errorP
    
__main_wrapper :: IO Unit__ -> P.IO ()
__main_wrapper m
  | Just x <- de_ioEH (unbox m) = x >> return ()
  | otherwise = P.error $ "__main_wrapper: " ++ pretty (unbox m)

numeric :: (Symbolic a) => a
numeric = primS numericP 

valueof :: (Symbolic a) => a -> Integer
valueof = primS valueofP

trace :: (Symbolic a) => String -> a -> a
trace = primS traceP

__prim_lt_Bit :: (Symbolic n) => Bit n -> Bit n -> Bool
__prim_lt_Bit = primS lt_BitP

__prim_leq_Bit :: (Symbolic n) => Bit n -> Bit n -> Bool
__prim_leq_Bit = primS leq_BitP

__prim_gt_Bit :: (Symbolic n) => Bit n -> Bit n -> Bool
__prim_gt_Bit = primS gt_BitP

__prim_geq_Bit :: (Symbolic n) => Bit n -> Bit n -> Bool
__prim_geq_Bit = primS geq_BitP

