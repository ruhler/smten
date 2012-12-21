
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}

module Seri.HaskellF.Lib.Prelude (
    Char,
    Integer,
    Bit,
    IO,
    Unit__, __mkUnit__, __caseUnit__,
    Bool, __mkTrue, __mkFalse, __caseTrue, __caseFalse,
    List__, __mkCons__, __mkNil__, __caseCons__, __caseNil__,

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

newtype Char = Char ExpH

instance SeriT Char where
    seriT _ = charT

instance Symbolic Char where
    box = Char
    unbox (Char x) = x

    
newtype Integer = Integer ExpH

instance SeriT Integer where
    seriT _ = integerT

instance Symbolic Integer where
    box = Integer
    unbox (Integer x) = x

instance Prelude.Num Integer where
    fromInteger = seriS
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
      | Just [] <- de_kconEH (name "()") e = Unit__
      | otherwise = Unit__s e

    unbox x
      | Unit__ <- x = conS' x "()" []
      | Unit__s v <- x = v

__mkUnit__ :: Unit__
__mkUnit__ = Unit__

__casesUnit__ :: (Symbolic a) => Unit__ -> a -> a -> a
__casesUnit__ = caseS "()"

__caseUnit__ :: (Symbolic a) => Unit__ -> a -> a -> a
__caseUnit__ x y n
  | Unit__ <- x = y
  | Unit__s _ <- x = __casesUnit__ x y n
  | otherwise = n

data Bool =
    True
  | False
  | Bool_s ExpH

instance SeriT Bool where
    seriT _ = boolT

instance Symbolic Bool where
    box e
      | Just [] <- de_kconEH (name "True") e = True
      | Just [] <- de_kconEH (name "False") e = False
      | otherwise = Bool_s e

    unbox x
      | True <- x = conS' x "True" []
      | False <- x = conS' x "False" []
      | Bool_s v <- x = v

__mkTrue :: Bool
__mkTrue = True

__mkFalse :: Bool
__mkFalse = False

__caseTrue :: (Symbolic a) => Bool -> a -> a -> a
__caseTrue x y n
  | True <- x = y
  | Bool_s _ <- x = __casesTrue x y n
  | otherwise = n

__caseFalse :: (Symbolic a) => Bool -> a -> a -> a
__caseFalse x y n
  | False <- x = y
  | Bool_s _ <- x = __casesFalse x y n
  | otherwise = n

__casesTrue :: (Symbolic a) => Bool -> a -> a -> a
__casesTrue = caseS "True"

__casesFalse :: (Symbolic a) => Bool -> a -> a -> a
__casesFalse = caseS "False"

data List__ a =
      Nil__ 
    | Cons__ a (List__ a)
    | List__s ExpH

instance SeriT1 List__ where
    seriT1 _ = seriT1 [()]

instance Symbolic1 List__ where
    box1 e
     | Just [] <- de_kconEH (name "[]") e = Nil__
     | Just [x, xs] <- de_kconEH (name ":") e = Cons__ (box x) (box xs)
     | otherwise = List__s e

    unbox1 x
     | Nil__ <- x = conS' x "[]" []
     | Cons__ a b <- x = conS' x ":" [unbox a, unbox b]
     | List__s v <- x = v

__mkNil__ :: (Symbolic a) => List__ a
__mkNil__ = Nil__

__mkCons__ :: (Symbolic a) => a -> List__ a -> List__ a
__mkCons__ = Cons__

__caseNil__ :: (Symbolic a, Symbolic z) => List__ a -> z -> z -> z
__caseNil__ x y n
  | Nil__ <- x = y
  | List__s _ <- x = __casesNil__ x y n
  | otherwise = n

__caseCons__ :: (Symbolic a, Symbolic z) => List__ a -> (a -> List__ a -> z) -> z -> z
__caseCons__ x y n
  | Cons__ a b <- x = y a b
  | List__s _ <- x = __casesCons__ x y n
  | otherwise = n

__casesNil__ :: (Symbolic a, Symbolic z) => List__ a -> z -> z -> z
__casesNil__ = caseS "[]"

__casesCons__ :: (Symbolic a, Symbolic z) => List__ a -> (a -> List__ a -> z) -> z -> z
__casesCons__ = caseS ":"

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
__prim_toInteger_Char = primS toInteger_CharP

__prim_eq_Integer :: Integer -> Integer -> Bool
__prim_eq_Integer = primS eq_IntegerP

__prim_add_Integer :: Integer -> Integer -> Integer
__prim_add_Integer = primS add_IntegerP

__prim_sub_Integer :: Integer -> Integer -> Integer
__prim_sub_Integer = primS sub_IntegerP

__prim_mul_Integer :: Integer -> Integer -> Integer
__prim_mul_Integer = primS mul_IntegerP

__prim_lt_Integer :: Integer -> Integer -> Bool
__prim_lt_Integer = primS lt_IntegerP

__prim_leq_Integer :: Integer -> Integer -> Bool
__prim_leq_Integer = primS leq_IntegerP

__prim_gt_Integer :: Integer -> Integer -> Bool
__prim_gt_Integer = primS gt_IntegerP

__prim_geq_Integer :: Integer -> Integer -> Bool
__prim_geq_Integer = primS geq_IntegerP

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

