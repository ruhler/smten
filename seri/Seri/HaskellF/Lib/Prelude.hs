
module Seri.HaskellF.Lib.Prelude (
    Symbolic__(..), Bool(), Char, Integer, IO, Bit, Unit__, List__,
    __mkUnit__, __caseUnit__,
    __mkTrue, __mkFalse, __caseTrue, __caseFalse,
    __mkCons__, __mkNil__, __caseCons__, __caseNil__,

    not, (&&), (||),
    __prim_eq_Char, __prim_eq_Integer, __prim_add_Integer, __prim_sub_Integer,
    __prim_mul_Integer, (<), (>),
    __prim_show_Integer,
    return_io, bind_io, nobind_io, fail_io, putChar,

    __prim_eq_Bit, __prim_show_Bit, __prim_add_Bit, __prim_sub_Bit, 
    __prim_mul_Bit, __prim_fromInteger_Bit, __prim_shl_Bit,
    __prim_lshr_Bit, __prim_or_Bit, __prim_and_Bit, __prim_not_Bit,
    __prim_zeroExtend_Bit, __prim_truncate_Bit, __prim_concat_Bit,
    __prim_extract_Bit,

    error,

    module Seri.Haskell.Lib.Numeric,
    ) where

import qualified Prelude
import qualified Seri.Haskell.Lib.Bit as Bit
import Seri.Haskell.Lib.Numeric

type Char = Prelude.Char
type Integer = Prelude.Integer
type IO = Prelude.IO
type Bit = Bit.Bit
type Unit__ = ()
type List__ = []

data Bool = True
          | False
          | Free Prelude.String
          | Conditional Bool Bool Bool

__mkTrue :: Bool
__mkTrue = True

__mkFalse :: Bool
__mkFalse = False

__caseTrue :: (Symbolic__ a) => Bool -> a -> a -> a
__caseTrue True y _ = y
__caseTrue False _ n = n
__caseTrue p y n = __if p y n

__caseFalse :: (Symbolic__ a) => Bool -> a -> a -> a
__caseFalse True _ n = n
__caseFalse False y _ = y
__caseFalse p y n = __if p n y

class Symbolic__ a where
    __if :: Bool -> a -> a -> a

instance Symbolic__ Bool where
    __if = Conditional 

not :: Bool -> Bool
not x = __caseTrue x __mkFalse __mkTrue

(&&) :: Bool -> Bool -> Bool
(&&) x y = __caseTrue x y __mkFalse

(||) :: Bool -> Bool -> Bool
(||) x y = __caseTrue x __mkTrue y

__prim_eq_Char :: Char -> Char -> Bool
__prim_eq_Char a b = if (a Prelude.== b) then __mkTrue else __mkFalse

__prim_eq_Integer :: Integer -> Integer -> Bool
__prim_eq_Integer a b = if (a Prelude.== b) then __mkTrue else __mkFalse

__prim_add_Integer :: Integer -> Integer -> Integer
__prim_add_Integer = (Prelude.+)

__prim_sub_Integer :: Integer -> Integer -> Integer
__prim_sub_Integer = (Prelude.-)

__prim_mul_Integer :: Integer -> Integer -> Integer
__prim_mul_Integer = (Prelude.*)

(<) :: Integer -> Integer -> Bool
(<) a b = if (a Prelude.< b) then __mkTrue else __mkFalse

(>) :: Integer -> Integer -> Bool
(>) a b = if (a Prelude.> b) then __mkTrue else __mkFalse

__prim_show_Integer :: Integer -> List__ Char
__prim_show_Integer = Prelude.show

return_io :: a -> IO a
return_io = Prelude.return

bind_io :: IO a -> (a -> IO b) -> IO b
bind_io = (Prelude.>>=)

nobind_io :: IO a -> IO b -> IO b
nobind_io = (Prelude.>>)

fail_io :: List__ Char -> IO a
fail_io = Prelude.fail

putChar :: Char -> IO ()
putChar = Prelude.putChar

__prim_eq_Bit :: (N__ n) => Bit n -> Bit n -> Bool
__prim_eq_Bit a b = if a Prelude.== b then __mkTrue else __mkFalse

__prim_show_Bit :: Bit n -> List__ Char
__prim_show_Bit = Prelude.show

__prim_add_Bit :: (N__ n) => Bit n -> Bit n -> Bit n
__prim_add_Bit = (Prelude.+)

__prim_sub_Bit :: (N__ n) => Bit n -> Bit n -> Bit n
__prim_sub_Bit = (Prelude.-)

__prim_mul_Bit :: (N__ n) => Bit n -> Bit n -> Bit n
__prim_mul_Bit = (Prelude.*)

__prim_fromInteger_Bit :: (N__ n) => Integer -> Bit n
__prim_fromInteger_Bit = Prelude.fromInteger

__prim_shl_Bit :: (N__ n) => Bit n -> Bit n -> Bit n
__prim_shl_Bit = Bit.shl

__prim_lshr_Bit :: (N__ n) => Bit n -> Bit n -> Bit n
__prim_lshr_Bit = Bit.lshr

__prim_or_Bit :: (N__ n) => Bit n -> Bit n -> Bit n
__prim_or_Bit = Bit.or

__prim_and_Bit :: (N__ n) => Bit n -> Bit n -> Bit n
__prim_and_Bit = Bit.and

__prim_not_Bit :: (N__ n) => Bit n -> Bit n
__prim_not_Bit = Bit.not

__prim_zeroExtend_Bit :: (N__ n, N__ m) => Bit n -> Bit m
__prim_zeroExtend_Bit = Bit.zeroExtend

__prim_truncate_Bit :: (N__ n, N__ m) => Bit n -> Bit m
__prim_truncate_Bit = Bit.truncate

__prim_concat_Bit :: (N__ a, N__ b) => Bit a -> Bit b -> Bit (N__PLUS a b)
__prim_concat_Bit = Bit.concat

__prim_extract_Bit :: (N__ n, N__ m) => Bit n -> Integer -> Bit m
__prim_extract_Bit = Bit.extract

error :: List__ Char -> a
error = Prelude.error

__mkCons__ :: a -> List__ a -> List__ a
__mkCons__ = (:)

__mkNil__ :: List__ a
__mkNil__ = []

__caseCons__ :: List__ a -> (a -> List__ a -> x) -> x -> x
__caseCons__ (x:xs) f _ = f x xs
__caseCons__ _ _ n = n

__caseNil__ :: List__ a -> x -> x -> x
__caseNil__ [] y _ = y
__caseNil__ _ _ n = n

__mkUnit__ :: Unit__
__mkUnit__ = ()

__caseUnit__ :: Unit__ -> a -> a -> a
__caseUnit__ () y _ = y

