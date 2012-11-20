
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}

module Seri.HaskellF.Lib.Prelude (
    Symbolic__(..), Symbolic1__(..), Symbolic2__(..), Symbolic3__(..),
    Symbolic4__(..), Symbolic5__(..), Symbolic6__(..), Symbolic7__(..),
    Symbolic8__(..), Symbolic9__(..),

    Bool(), Char, Integer, IO, Bit, Unit__, List__,
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

    N__, module NE,
    ) where

import qualified Prelude
import qualified Seri.Haskell.Lib.Bit as Bit
import Seri.Haskell.Lib.Numeric as NE hiding (N__) 
import qualified Seri.Haskell.Lib.Numeric as N
import qualified Seri.SMT.Syntax as SMT

type Char = Prelude.Char
type Integer = Prelude.Integer
type IO = Prelude.IO
type Bit = Bit.Bit
type Unit__ = ()
type List__ = []

newtype Bool = Bool (SMT.Expression)
    deriving(Prelude.Show)

__mkTrue :: Bool
__mkTrue = Bool SMT.trueE

__mkFalse :: Bool
__mkFalse = Bool SMT.falseE

__caseTrue :: (Symbolic__ a) => Bool -> a -> a -> a
__caseTrue p a b = __if p a b

__caseFalse :: (Symbolic__ a) => Bool -> a -> a -> a
__caseFalse p y n = __if p n y

class Symbolic__ a where
    __if :: Bool -> a -> a -> a
    __if (Bool p) a b
        | Prelude.Just Prelude.True <- SMT.de_boolE p = a
        | Prelude.Just Prelude.False <- SMT.de_boolE p = b
        | Prelude.otherwise = Prelude.error ("Unsupported __if predicate: ?")

    __default :: a

class Symbolic1__ m where
    __if1 :: (Symbolic__ a) => Bool -> m a -> m a -> m a
    __if1 (Bool p) a b
        | Prelude.Just Prelude.True <- SMT.de_boolE p = a
        | Prelude.Just Prelude.False <- SMT.de_boolE p = b
        | Prelude.otherwise = Prelude.error ("Unsupported __if1 predicate: ?")

    __default1 :: (Symbolic__ a) => m a

instance (Symbolic1__ m, Symbolic__ a) => Symbolic__ (m a) where
    __if = __if1
    __default = __default1

class Symbolic2__ m where
    __if2 :: (Symbolic__ a, Symbolic__ b) =>
        Bool -> m a b -> m a b -> m a b
    __if2 (Bool p) a b
        | Prelude.Just Prelude.True <- SMT.de_boolE p = a
        | Prelude.Just Prelude.False <- SMT.de_boolE p = b
        | Prelude.otherwise = Prelude.error ("Unsupported __if2 predicate: ?")

    __default2 :: (Symbolic__ a, Symbolic__ b) => m a b


instance (Symbolic2__ m, Symbolic__ a) => Symbolic1__ (m a) where
    __if1 = __if2
    __default1 = __default2

class Symbolic3__ m where
    __if3 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c) =>
        Bool -> m a b c -> m a b c -> m a b c
    __if3 (Bool p) a b
        | Prelude.Just Prelude.True <- SMT.de_boolE p = a
        | Prelude.Just Prelude.False <- SMT.de_boolE p = b
        | Prelude.otherwise = Prelude.error ("Unsupported __if3 predicate: ?")

    __default3 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c) => m a b c

instance (Symbolic3__ m, Symbolic__ a) => Symbolic2__ (m a) where
    __if2 = __if3
    __default2 = __default3

class Symbolic4__ m where
    __if4 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d) =>
        Bool -> m a b c d -> m a b c d -> m a b c d
    __default4 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d) =>
        m a b c d

instance (Symbolic4__ m, Symbolic__ a) => Symbolic3__ (m a) where
    __if3 = __if4
    __default3 = __default4

class Symbolic5__ m where
    __if5 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d,
              Symbolic__ e) =>
        Bool -> m a b c d e -> m a b c d e -> m a b c d e

    __default5 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d,
              Symbolic__ e) => m a b c d e 

instance (Symbolic5__ m, Symbolic__ a) => Symbolic4__ (m a) where
    __if4 = __if5
    __default4 = __default5

class Symbolic6__ m where
    __if6 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d,
              Symbolic__ e, Symbolic__ f) =>
        Bool -> m a b c d e f -> m a b c d e f -> m a b c d e f
    __default6 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d,
              Symbolic__ e, Symbolic__ f) => m a b c d e f

instance (Symbolic6__ m, Symbolic__ a) => Symbolic5__ (m a) where
    __if5 = __if6
    __default5 = __default6

class Symbolic7__ m where
    __if7 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d,
              Symbolic__ e, Symbolic__ f, Symbolic__ g) =>
        Bool -> m a b c d e f g -> m a b c d e f g -> m a b c d e f g
    __default7 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d,
              Symbolic__ e, Symbolic__ f, Symbolic__ g) => m a b c d e f g 

instance (Symbolic7__ m, Symbolic__ a) => Symbolic6__ (m a) where
    __if6 = __if7
    __default6 = __default7

class Symbolic8__ m where
    __if8 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d,
              Symbolic__ e, Symbolic__ f, Symbolic__ g, Symbolic__ h) =>
        Bool -> m a b c d e f g h -> m a b c d e f g h -> m a b c d e f g h
    __default8 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d,
              Symbolic__ e, Symbolic__ f, Symbolic__ g, Symbolic__ h) =>
                m a b c d e f g h 

instance (Symbolic8__ m, Symbolic__ a) => Symbolic7__ (m a) where
    __if7 = __if8
    __default7 = __default8

class Symbolic9__ m where
    __if9 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d,
              Symbolic__ e, Symbolic__ f, Symbolic__ g, Symbolic__ h,
              Symbolic__ i) =>
        Bool -> m a b c d e f g h i -> m a b c d e f g h i -> m a b c d e f g h i

    __default9 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d,
              Symbolic__ e, Symbolic__ f, Symbolic__ g, Symbolic__ h,
              Symbolic__ i) => m a b c d e f g h i

instance (Symbolic9__ m, Symbolic__ a) => Symbolic8__ (m a) where
    __if8 = __if9
    __default8 = __default9

instance Symbolic__ Bool where
    __if (Bool p) a@(Bool ax) b@(Bool bx)
        | Prelude.Just Prelude.True <- SMT.de_boolE p = a
        | Prelude.Just Prelude.False <- SMT.de_boolE p = b
        | Prelude.otherwise = Bool (SMT.ifE p ax bx)

    __default = __mkFalse

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

instance Symbolic__ Unit__ where
    __if _ _ _ = ()
    __default = ()

instance Symbolic2__ (->) where
    __if2 p f g = \x -> __if p (f x) (g x)
    __default2 = \_ -> __default

instance Symbolic__ Char where
    __default = '?'

instance Symbolic__ Integer where
    __default = 0

instance Symbolic__ N__0 where
    __default = N__0

instance Symbolic1__ IO where
    __default1 = return_io __default

instance Symbolic1__ List__ where
    __default1 = [__default]

instance Symbolic1__ Bit where
    __default1 = Prelude.error "TODO: default1 Bit"

instance Symbolic1__ N__2p0 where
    __default1 = N__2p0 __default

instance Symbolic1__ N__2p1 where
    __default1 = N__2p1 __default

instance Symbolic2__ N__PLUS where
    __default2 = N__PLUS __default __default

instance Symbolic2__ N__MINUS where
    __default2 = N__MINUS __default __default

instance Symbolic2__ N__TIMES where
    __default2 = N__TIMES __default __default

class (Symbolic__ a, N.N__ a) => N__ a where

instance N__ N__0 where
instance (N__ n) => N__ (N__2p0 n) where
instance (N__ n) => N__ (N__2p1 n) where
instance (N__ a, N__ b) => N__ (N__PLUS a b) where
instance (N__ a, N__ b) => N__ (N__MINUS a b) where
instance (N__ a, N__ b) => N__ (N__TIMES a b) where

