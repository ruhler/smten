
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}

module Seri.HaskellF.Lib.Prelude (
    Symbolic__(..), Symbolic1__(..), Symbolic2__(..), Symbolic3__(..),
    Symbolic4__(..), Symbolic5__(..), Symbolic6__(..), Symbolic7__(..),
    Symbolic8__(..), Symbolic9__(..),

    Bool(..), Char, Integer(..), IO, Bit, Unit__, List__,
    __concrete,
    __mkUnit__, __caseUnit__,
    __mkTrue, __mkFalse, __caseTrue, __caseFalse,
    __mkCons__, __mkNil__, __caseCons__, __caseNil__,

    not, (&&), (||),
    __prim_eq_Char, __prim_eq_Integer, __prim_add_Integer, __prim_sub_Integer,
    __prim_mul_Integer, (<), (<=), (>),
    __prim_show_Integer,
    return_io, bind_io, nobind_io, fail_io, putChar, getContents,

    __prim_eq_Bit, __prim_show_Bit, __prim_add_Bit, __prim_sub_Bit, 
    __prim_mul_Bit, __prim_fromInteger_Bit, __prim_shl_Bit,
    __prim_lshr_Bit, __prim_or_Bit, __prim_and_Bit, __prim_not_Bit,
    __prim_zeroExtend_Bit, __prim_truncate_Bit, __prim_concat_Bit,
    __prim_extract_Bit,

    error,

    N__(..), module NE,

    __if_default,
    __list,
    __errorh,
    ) where

import Prelude((.), ($), (++))
import qualified Prelude
import Data.Functor ((<$>))
import Data.Maybe (fromMaybe)

import qualified Seri.Haskell.Lib.Bit as Bit
import Seri.Haskell.Lib.Numeric as NE hiding (N__(..)) 
import qualified Seri.Haskell.Lib.Numeric as N
import qualified Seri.Name as S
import qualified Seri.Exp as S
import qualified Seri.Ppr as S

type IO = Prelude.IO
type Bit = Bit.Bit
type Unit__ = ()
type Char = Concrete__ Prelude.Char

newtype Bool = Bool S.Exp
    deriving(Prelude.Show)

newtype Integer = Integer S.Exp
    deriving(Prelude.Show)

mkInteger :: Prelude.Integer -> Integer
mkInteger = Integer . S.integerE

mkBool :: Prelude.Bool -> Bool
mkBool = Bool . S.boolE

data Concrete__ a = Concrete_c a
                  | Concrete_if Bool (Concrete__ a) (Concrete__ a)
    deriving (Prelude.Show)

__concrete :: a -> Concrete__ a
__concrete = Concrete_c

__de_concrete :: Concrete__ a -> Prelude.Maybe a
__de_concrete (Concrete_c a) = Prelude.Just a
__de_concrete _ = Prelude.Nothing

-- Apply a function to a concrete argument.
__capp :: (Symbolic__ b) => (a -> b) -> Concrete__ a -> b
__capp f (Concrete_c x) = f x
__capp f (Concrete_if p a b) = __if p (__capp f a) (__capp f b)

__capp2 :: (Symbolic__ a, Symbolic__ b) => (a -> a -> b)
                          -> Concrete__ a -> Concrete__ a -> b
__capp2 f = __capp . __capp f

data List__ a = List_c [a]
              | List_if Bool (List__ a) (List__ a)
    deriving (Prelude.Show)

__list :: [a] -> List__ a
__list = List_c

__de_list :: List__ a -> Prelude.Maybe [a]
__de_list (List_c x) = Prelude.return x
__de_list _ = Prelude.Nothing

__errorh :: List__ Char -> a
__errorh msg = fromMaybe (Prelude.error (Prelude.show msg)) $ do
  l <- __de_list msg
  str <- Prelude.mapM __de_concrete l
  Prelude.return (Prelude.error str)



instance Prelude.Num Integer where
    fromInteger = mkInteger
    (+) = Prelude.error $ "+ for haskellf Integer not defined"
    (*) = Prelude.error $ "* for haskellf Integer not defined"
    abs = Prelude.error $ "abs for haskellf Integer not defined"
    signum = Prelude.error $ "signum for haskellf Integer not defined"


__mkTrue :: Bool
__mkTrue = Bool S.trueE

__mkFalse :: Bool
__mkFalse = Bool S.falseE

__caseTrue :: (Symbolic__ a) => Bool -> a -> a -> a
__caseTrue p a b = __if p a b

__caseFalse :: (Symbolic__ a) => Bool -> a -> a -> a
__caseFalse p y n = __if p n y

class Symbolic__ a where
    __if :: Bool -> a -> a -> a
    __default :: a
    __error :: List__ Char -> a
    __error = Prelude.const __default
    __substitute :: (S.Name -> Prelude.Maybe S.Exp) -> a -> a
    __substitute _ = Prelude.id

class Symbolic1__ m where
    __if1 :: (Symbolic__ a) => Bool -> m a -> m a -> m a
    __default1 :: (Symbolic__ a) => m a
    __error1 :: (Symbolic__ a) => List__ Char -> m a
    __error1 = Prelude.const __default1
    __substitute1 :: (Symbolic__ a) => (S.Name -> Prelude.Maybe S.Exp) -> m a -> m a
    __substitute1 _ = Prelude.id

instance (Symbolic1__ m, Symbolic__ a) => Symbolic__ (m a) where
    __if = __if1
    __default = __default1
    __error = __error1
    __substitute = __substitute1

class Symbolic2__ m where
    __if2 :: (Symbolic__ a, Symbolic__ b) =>
        Bool -> m a b -> m a b -> m a b

    __default2 :: (Symbolic__ a, Symbolic__ b) => m a b
    __error2 :: (Symbolic__ a, Symbolic__ b) => List__ Char -> m a b
    __error2 = Prelude.const __default2
    __substitute2 :: (Symbolic__ a, Symbolic__ b) =>
        (S.Name -> Prelude.Maybe S.Exp) -> m a b -> m a b
    __substitute2 _ = Prelude.id


instance (Symbolic2__ m, Symbolic__ a) => Symbolic1__ (m a) where
    __if1 = __if2
    __default1 = __default2
    __error1 = __error2
    __substitute1 = __substitute2

class Symbolic3__ m where
    __if3 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c) =>
        Bool -> m a b c -> m a b c -> m a b c

    __default3 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c) => m a b c
    __error3 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c) => List__ Char -> m a b c
    __error3 = Prelude.const __default3
    __substitute3 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c) =>
        (S.Name -> Prelude.Maybe S.Exp) -> m a b c -> m a b c

instance (Symbolic3__ m, Symbolic__ a) => Symbolic2__ (m a) where
    __if2 = __if3
    __default2 = __default3
    __error2 = __error3
    __substitute2 = __substitute3

class Symbolic4__ m where
    __if4 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d) =>
        Bool -> m a b c d -> m a b c d -> m a b c d
    __default4 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d) => m a b c d
    __error4 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d) => List__ Char -> m a b c d
    __error4 = Prelude.const __default4
    __substitute4 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d) =>
        (S.Name -> Prelude.Maybe S.Exp) -> m a b c d -> m a b c d

instance (Symbolic4__ m, Symbolic__ a) => Symbolic3__ (m a) where
    __if3 = __if4
    __default3 = __default4
    __error3 = __error4
    __substitute3 = __substitute4

class Symbolic5__ m where
    __if5 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e) =>
        Bool -> m a b c d e -> m a b c d e -> m a b c d e

    __default5 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e) => m a b c d e 
    __error5 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e) => List__ Char -> m a b c d e 
    __error5 = Prelude.const __default5
    __substitute5 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e) =>
        (S.Name -> Prelude.Maybe S.Exp) -> m a b c d e -> m a b c d e

instance (Symbolic5__ m, Symbolic__ a) => Symbolic4__ (m a) where
    __if4 = __if5
    __default4 = __default5
    __error4 = __error5
    __substitute4 = __substitute5

class Symbolic6__ m where
    __if6 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e, Symbolic__ f) =>
        Bool -> m a b c d e f -> m a b c d e f -> m a b c d e f
    __default6 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e, Symbolic__ f) => m a b c d e f
    __error6 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e, Symbolic__ f) => List__ Char -> m a b c d e f
    __error6 = Prelude.const __default6
    __substitute6 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e, Symbolic__ f) =>
        (S.Name -> Prelude.Maybe S.Exp) -> m a b c d e f -> m a b c d e f

instance (Symbolic6__ m, Symbolic__ a) => Symbolic5__ (m a) where
    __if5 = __if6
    __default5 = __default6
    __error5 = __error6
    __substitute5 = __substitute6

class Symbolic7__ m where
    __if7 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e, Symbolic__ f, Symbolic__ g) =>
        Bool -> m a b c d e f g -> m a b c d e f g -> m a b c d e f g
    __default7 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e, Symbolic__ f, Symbolic__ g) => m a b c d e f g 
    __error7 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e, Symbolic__ f, Symbolic__ g) => List__ Char -> m a b c d e f g 
    __error7 = Prelude.const __default7
    __substitute7 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e, Symbolic__ f, Symbolic__ g) =>
        (S.Name -> Prelude.Maybe S.Exp) -> m a b c d e f g -> m a b c d e f g

instance (Symbolic7__ m, Symbolic__ a) => Symbolic6__ (m a) where
    __if6 = __if7
    __default6 = __default7
    __error6 = __error7
    __substitute6 = __substitute7

class Symbolic8__ m where
    __if8 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e, Symbolic__ f, Symbolic__ g, Symbolic__ h) =>
        Bool -> m a b c d e f g h -> m a b c d e f g h -> m a b c d e f g h
    __default8 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e, Symbolic__ f, Symbolic__ g, Symbolic__ h) => m a b c d e f g h 
    __error8 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e, Symbolic__ f, Symbolic__ g, Symbolic__ h) => List__ Char -> m a b c d e f g h 
    __error8 = Prelude.const __default8
    __substitute8 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e, Symbolic__ f, Symbolic__ g, Symbolic__ h) =>
        (S.Name -> Prelude.Maybe S.Exp) -> m a b c d e f g h -> m a b c d e f g h

instance (Symbolic8__ m, Symbolic__ a) => Symbolic7__ (m a) where
    __if7 = __if8
    __default7 = __default8
    __error7 = __error8
    __substitute7 = __substitute8

class Symbolic9__ m where
    __if9 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e, Symbolic__ f, Symbolic__ g, Symbolic__ h, Symbolic__ i) =>
        Bool -> m a b c d e f g h i -> m a b c d e f g h i -> m a b c d e f g h i

    __default9 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d,
              Symbolic__ e, Symbolic__ f, Symbolic__ g, Symbolic__ h,
              Symbolic__ i) => m a b c d e f g h i
    __error9 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d,
              Symbolic__ e, Symbolic__ f, Symbolic__ g, Symbolic__ h,
              Symbolic__ i) => List__ Char -> m a b c d e f g h i
    __error9 = Prelude.const __default9
    __substitute9 :: (Symbolic__ a, Symbolic__ b, Symbolic__ c, Symbolic__ d, Symbolic__ e, Symbolic__ f, Symbolic__ g, Symbolic__ h, Symbolic__ i) =>
        (S.Name -> Prelude.Maybe S.Exp) -> m a b c d e f g h i -> m a b c d e f g h i

instance (Symbolic9__ m, Symbolic__ a) => Symbolic8__ (m a) where
    __if8 = __if9
    __default8 = __default9
    __error8 = __error9
    __substitute8 = __substitute9

instance Symbolic__ Bool where
    __if (Bool p) a@(Bool ax) b@(Bool bx)
        | Prelude.Just Prelude.True <- S.de_boolE p = a
        | Prelude.Just Prelude.False <- S.de_boolE p = b
        | Prelude.otherwise =
           let a' = S.simplify $ S.substitute (Prelude.flip Prelude.lookup (S.impliedByTrue p)) ax
               b' = S.simplify $ S.substitute (Prelude.flip Prelude.lookup (S.impliedByFalse p)) bx
           in Bool (S.ifE p a' b')

    __default = __mkFalse
    __substitute f (Bool x) = Bool (S.simplify $ S.substitute f x)

not :: Bool -> Bool
not x = __caseTrue x __mkFalse __mkTrue

(&&) :: Bool -> Bool -> Bool
(&&) x y = __caseTrue x y __mkFalse

(||) :: Bool -> Bool -> Bool
(||) x y = __caseTrue x __mkTrue y

__prim_eq_Char :: Char -> Char -> Bool
__prim_eq_Char = __capp2 $ \a b -> mkBool (a Prelude.== b)

__prim_eq_Integer :: Integer -> Integer -> Bool
__prim_eq_Integer (Integer a) (Integer b) = Bool $ S.integer_eqE a b

__prim_add_Integer :: Integer -> Integer -> Integer
__prim_add_Integer (Integer a) (Integer b) = Integer $ S.integer_addE a b

__prim_sub_Integer :: Integer -> Integer -> Integer
__prim_sub_Integer (Integer a) (Integer b) = Integer $ S.integer_subE a b

__prim_mul_Integer :: Integer -> Integer -> Integer
__prim_mul_Integer (Integer a) (Integer b) = Integer $ S.integer_mulE a b

(<) :: Integer -> Integer -> Bool
(<) (Integer a) (Integer b) = Bool $ S.integer_ltE a b

(<=) :: Integer -> Integer -> Bool
(<=) (Integer a) (Integer b) = Bool $ S.integer_leqE a b

(>) :: Integer -> Integer -> Bool
(>) (Integer a) (Integer b) = Bool $ S.integer_gtE a b

__prim_show_Integer :: Integer -> List__ Char
__prim_show_Integer = __list . Prelude.map __concrete . Prelude.show

return_io :: a -> IO a
return_io = Prelude.return

bind_io :: IO a -> (a -> IO b) -> IO b
bind_io = (Prelude.>>=)

nobind_io :: IO a -> IO b -> IO b
nobind_io = (Prelude.>>)

fail_io :: List__ Char -> IO a
fail_io = Prelude.error $ "TODO: haskellf fail_io"

putChar :: Char -> IO ()
putChar = __capp Prelude.putChar

getContents :: IO (List__ Char)
getContents = __string <$> Prelude.getContents

__string :: Prelude.String -> List__ Char
__string = __list . Prelude.map __concrete

__prim_eq_Bit :: (N__ n) => Bit n -> Bit n -> Bool
__prim_eq_Bit a b = if a Prelude.== b then __mkTrue else __mkFalse

__prim_show_Bit :: Bit n -> List__ Char
__prim_show_Bit = __string . Prelude.show

__prim_add_Bit :: (N__ n) => Bit n -> Bit n -> Bit n
__prim_add_Bit = (Prelude.+)

__prim_sub_Bit :: (N__ n) => Bit n -> Bit n -> Bit n
__prim_sub_Bit = (Prelude.-)

__prim_mul_Bit :: (N__ n) => Bit n -> Bit n -> Bit n
__prim_mul_Bit = (Prelude.*)

__prim_fromInteger_Bit :: (N__ n) => Integer -> Bit n
__prim_fromInteger_Bit (Integer a)
  | Prelude.Just v <- S.de_integerE a = Prelude.fromInteger v
  | Prelude.otherwise = Prelude.error $ "TODO: __prim_fromInteger_Bit for haskellf: " ++ S.pretty a

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
__prim_extract_Bit b (Integer a)
  | Prelude.Just v <- S.de_integerE a = Bit.extract b v
  | Prelude.otherwise = Prelude.error "TODO: __prim_extract_Bit for haskellf"

error :: (Symbolic__ a) => List__ Char -> a
error = __error

__mkCons__ :: (Symbolic__ a) => a -> List__ a -> List__ a
__mkCons__ x (List_c xs) = __list (x:xs)
__mkCons__ x (List_if p a b) = __if p (__mkCons__ x a) (__mkCons__ x b)

__mkNil__ :: List__ a
__mkNil__ = __list []

__caseCons__ :: (Symbolic__ x) => List__ a -> (a -> List__ a -> x) -> x -> x
__caseCons__ (List_c (x:xs)) f _ = f x (List_c xs)
__caseCons__ (List_c []) _ n = n
__caseCons__ (List_if p a b) y n = __if p (__caseCons__ a y n) (__caseCons__ b y n)

__caseNil__ :: (Symbolic__ x) => List__ a -> x -> x -> x
__caseNil__ (List_c []) y _ = y
__caseNil__ (List_c _) _ n = n
__caseNil__ (List_if p a b) y n = __if p (__caseNil__ a y n) (__caseNil__ b y n)

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
    __substitute2 l f = \x -> __substitute l (f x)

instance Symbolic1__ Concrete__ where
    __default1 = __concrete __default
    __if1 p@(Bool px) a b
        | Prelude.Just Prelude.True <- S.de_boolE px = a
        | Prelude.Just Prelude.False <- S.de_boolE px = b
        | Prelude.otherwise = Concrete_if p a b

instance Symbolic__ Prelude.Char where
    __default = '?'
    __if = __if_default "Prelude.Char"

instance Symbolic1__ [] where
    __default1 = []
    __if1 = __if_default "Prelude.[]"
    __substitute1 f = Prelude.map (__substitute f)

instance Symbolic__ Prelude.Integer where
    __default = 0
    __if = __if_default "Prelude.Integer"

instance Symbolic__ Integer where
    __default = 0
    __if (Bool p) a@(Integer ax) b@(Integer bx)
        | Prelude.Just Prelude.True <- S.de_boolE p = a
        | Prelude.Just Prelude.False <- S.de_boolE p = b
        | Prelude.otherwise =
           let a' = S.simplify $ S.substitute (Prelude.flip Prelude.lookup (S.impliedByTrue p)) ax
               b' = S.simplify $ S.substitute (Prelude.flip Prelude.lookup (S.impliedByFalse p)) bx
           in Integer (S.ifE p a' b')
    __substitute f (Integer x) = Integer (S.simplify $ S.substitute f x)

instance Symbolic__ N__0 where
    __if = __if_default "N__0"
    __default = N__0

instance Symbolic1__ IO where
    __if1 = __if_default "IO"
    __default1 = return_io __default
    __error1 = __errorh 

instance Symbolic1__ List__ where
    __default1 = __list [__default]
    __if1 p@(Bool px) a b
        | Prelude.Just Prelude.True <- S.de_boolE px = a
        | Prelude.Just Prelude.False <- S.de_boolE px = b
        | Prelude.otherwise = List_if p a b
    __substitute1 f (List_c x) = List_c (__substitute1 f x)
    __substitute1 f (List_if p a b)
        = __if (__substitute f p) (__substitute f a) (__substitute f b)

instance Symbolic1__ Bit where
    __if1 = __if_default "Bit"
    __default1 = Prelude.error "TODO: default1 Bit"

instance Symbolic1__ N__2p0 where
    __if1 = __if_default "N__2p0"
    __default1 = N__2p0 __default

instance Symbolic1__ N__2p1 where
    __if1 = __if_default "N__2p1"
    __default1 = N__2p1 __default

instance Symbolic2__ N__PLUS where
    __if2 = __if_default "N__PLUS"
    __default2 = N__PLUS __default __default

instance Symbolic2__ N__MINUS where
    __if2 = __if_default "N__MINUS"
    __default2 = N__MINUS __default __default

instance Symbolic2__ N__TIMES where
    __if2 = __if_default "N__TIMES"
    __default2 = N__TIMES __default __default

__if_default :: Prelude.String -> Bool -> a -> a -> a
__if_default msg (Bool p) a b
    | Prelude.Just Prelude.True <- S.de_boolE p = a
    | Prelude.Just Prelude.False <- S.de_boolE p = b
    | Prelude.otherwise = Prelude.error ("__if " ++ msg ++ ": " ++ S.pretty p)

class (Symbolic__ a, N.N__ a) => N__ a where
    valueof :: a -> Integer
    valueof = mkInteger . N.valueof

    numeric :: a
    numeric = N.numeric

instance N__ N__0 where
instance (N__ n) => N__ (N__2p0 n) where
instance (N__ n) => N__ (N__2p1 n) where
instance (N__ a, N__ b) => N__ (N__PLUS a b) where
instance (N__ a, N__ b) => N__ (N__MINUS a b) where
instance (N__ a, N__ b) => N__ (N__TIMES a b) where

