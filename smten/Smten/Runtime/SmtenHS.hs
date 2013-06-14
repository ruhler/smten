
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Smten.Runtime.SmtenHS where

import Prelude hiding (Bool(..), Integer)
import qualified Prelude as Prelude
import qualified Prelude as P
import qualified Smten.Bit as P

import GHC.Base (Any)
import System.Mem.StableName
import System.IO.Unsafe
import Unsafe.Coerce

import Data.Bits
import Data.Dynamic
import Data.Maybe(fromMaybe)

import qualified Data.HashTable.IO as H

import Smten.SMT.FreeID
import Smten.CodeGen.TH

data Cases a =
    Concrete a
  | Switch Bool (Cases a) (Cases a)

concrete :: a -> Cases a
concrete = Concrete

switch :: Bool -> Cases a -> Cases a -> Cases a
switch = Switch

instance Functor Cases where
    fmap f (Concrete x) = Concrete (f x)
    fmap f (Switch p a b) = Switch p (fmap f a) (fmap f b)

f2map :: (a -> b -> c) -> Cases a -> Cases b -> Cases c
f2map f (Concrete a) y = fmap (f a) y
f2map f (Switch p a b) y = Switch p (f2map f a y) (f2map f b y)

f3map :: (a -> b -> c -> d) -> Cases a -> Cases b -> Cases c -> Cases d
f3map f (Concrete a) y z = f2map (f a) y z
f3map f (Switch p a b) y z = Switch p (f3map f a y z) (f3map f b y z)

data Bool where
    False :: Bool
    True :: Bool
    Bool_Var :: FreeID -> Bool
    Bool_EqInteger :: Integer -> Integer -> Bool
    Bool_LeqInteger :: Integer -> Integer -> Bool
    Bool_EqBit :: (SmtenHS0 n) => Bit n -> Bit n -> Bool
    Bool_LeqBit :: (SmtenHS0 n) => Bit n -> Bit n -> Bool
    Bool_Ite :: Bool -> Bool -> Bool -> Bool
    Bool_Prim :: (Assignment -> Bool) -> Cases Bool -> Bool
    Bool_Error :: String -> Bool

data Integer =
    Integer P.Integer
  | Integer_Add Integer Integer
  | Integer_Sub Integer Integer
  | Integer_Ite Bool Integer Integer
  | Integer_Var FreeID
  | Integer_Prim (Assignment -> Integer) (Cases Integer)
  | Integer_Error String

data Bit n where
    Bit :: P.Bit -> Bit n
    Bit_Add :: Bit n -> Bit n -> Bit n
    Bit_Sub :: Bit n -> Bit n -> Bit n
    Bit_Mul :: Bit n -> Bit n -> Bit n
    Bit_Or :: Bit n -> Bit n -> Bit n
    Bit_And :: Bit n -> Bit n -> Bit n
    Bit_Shl :: Bit n -> Bit n -> Bit n
    Bit_Lshr :: Bit n -> Bit n -> Bit n
    Bit_Concat :: (SmtenHS0 a, SmtenHS0 b) => Bit a -> Bit b -> Bit n
    Bit_Extract :: (SmtenHS0 a) => Bit a -> Integer -> Bit n
    Bit_Not :: Bit n -> Bit n
    Bit_SignExtend :: (SmtenHS0 m) => Bit m -> Bit n
    Bit_Ite :: Bool -> Bit n -> Bit n -> Bit n
    Bit_Var :: FreeID -> Bit n
    Bit_Prim :: (Assignment -> Bit n) -> Cases (Bit n) -> Bit n
    Bit_Error :: String -> Bit n

data Assignment = Assignment {
   as_vars :: [(FreeID, Dynamic)],
   as_cache :: H.BasicHashTable (StableName Any) Any
}

-- Give a nicer name to 'undefined' to use for specifying the argument to
-- valueof.
numeric :: a
numeric = error "numeric"

as_lookup :: (Typeable a) => FreeID -> Assignment -> a
as_lookup x m = fromMaybe (error "as_lookup failed") $ do
    v <- lookup x (as_vars m)
    fromDynamic v

as_make :: [(FreeID, Dynamic)] -> IO Assignment
as_make vars = do
    cache <- H.new
    return (Assignment vars cache)
  
realize :: (SmtenHS0 a) => Assignment -> a -> a
realize m x = unsafeDupablePerformIO $ do
    xnm <- makeStableName $! (unsafeCoerce x)
    let mc = as_cache m
    xfnd <- H.lookup mc xnm
    case xfnd of
      Just v -> return (unsafeCoerce v)
      Nothing -> do
        let v = realize0 m x
        H.insert mc xnm (unsafeCoerce v)
        return v

class SmtenHS0 a where
    -- Update all variables in the given expression according to the given map.
    realize0 :: Assignment -> a -> a

    -- Return the cases of 'a'.
    cases0 :: a -> Cases a

    error0 :: String -> a

    -- Represent a primitive function resulting in the given object.
    primitive0 :: (Assignment -> a) -> Cases a -> a

    __caseTrue0 :: Bool -> a -> a -> a
    __caseTrue0 x y n =
        case x of
            True -> y
            False -> n
            _ -> primitive0 (\m -> __caseTrue0 (realize m x) (realize m y) (realize m n))
                            (switch x (cases0 y) (cases0 n))

    -- For numeric types, this returns the value of the type.
    -- For other types, this is undefined.
    valueof0 :: a -> P.Integer
    valueof0 = error "valueof0 for non-numeric type"

declare_SmtenHS 1
declare_SmtenHS 2
declare_SmtenHS 3
declare_SmtenHS 4

derive_SmtenHS 0
derive_SmtenHS 1
derive_SmtenHS 2
derive_SmtenHS 3

-- Convenience functions for unsupported primitives.
--  f - a symbolic function which knows how to handle concrete arguments.
--  x - a symbolic argument which 'f' can't handle.
prim1 :: (SmtenHS0 a, SmtenHS0 b) => (a -> b) -> a -> b
prim1 f x = primitive0 (\m -> f (realize m x)) (fmap f (cases0 x))

prim3 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c, SmtenHS0 d) => (a -> b -> c -> d) -> a -> b -> c -> d
prim3 f x y z = primitive0 (\m -> f (realize m x) (realize m y) (realize m z))
                           (f3map f (cases0 x) (cases0 y) (cases0 z))

sprim1 :: (Haskelly ha sa, Haskelly hb sb) =>
          (ha -> hb) -> (sa -> sb) -> sa -> sb
sprim1 hf sf a
  | Just av <- mtohs a = frhs (hf av) 
  | otherwise = sf a

sprim2 :: (Haskelly ha sa, Haskelly hb sb, Haskelly hc sc) =>
          (ha -> hb -> hc) -> (sa -> sb -> sc) -> sa -> sb -> sc
sprim2 hf sf a b 
  | Just av <- mtohs a, Just bv <- mtohs b = frhs (hf av bv) 
  | otherwise = sf a b

class Haskelly h s where
    -- Convert from a haskell object to a smten object.
    frhs :: h -> s

    -- Maybe convert from a smten object to a haskell object.
    -- Returns Nothing if the smten object can't be represented as a haskell
    -- object.
    mtohs :: s -> Maybe h
    mtohs = return . stohs

    -- Surely convert from a smten object to a haskell object.
    -- Behavior is undefined if the smten object can't be represented as a
    -- haskell object.
    stohs :: s -> h
    stohs = fromMaybe (error "stohs") . mtohs

instance Haskelly a a where
    frhs = id
    mtohs = return
    stohs = id

instance SmtenHS2 (->) where
   realize2 m f = \x -> realize m (f x)
   cases2 f = concrete f
   primitive2 r c = \x -> primitive0 (\m -> r m $ realize m x) (fmap ($ x) c)
   error2 msg = \x -> error0 msg

instance (Haskelly ha sa, Haskelly hb sb, SmtenHS0 sa, SmtenHS0 sb)
         => Haskelly (ha -> hb) (sa -> sb) where
    frhs hf sx 
      | Just hx <- mtohs sx = frhs (hf hx)
      | otherwise = prim1 (frhs hf) sx
    stohs sf = \hx -> stohs $ sf (frhs hx)

instance Haskelly (a -> b) (a -> b) where
    frhs = id
    stohs = id

__caseTrue :: (SmtenHS0 z) => Bool -> z -> z -> z
__caseTrue = __caseTrue0

__caseFalse :: (SmtenHS0 z) => Bool -> z -> z -> z
__caseFalse x y n = __caseTrue x n y

instance SmtenHS0 Bool where
   realize0 m b@True = b
   realize0 m b@False = b
   realize0 m b@(Bool_Error {}) = b
   realize0 m (Bool_Var x) = frhs (as_lookup x m :: P.Bool)
   realize0 m (Bool_EqInteger a b) = eq_Integer (realize m a) (realize m b)
   realize0 m (Bool_LeqInteger a b) = leq_Integer (realize m a) (realize m b)
   realize0 m (Bool_EqBit a b) = eq_Bit (realize m a) (realize m b)
   realize0 m (Bool_LeqBit a b) = leq_Bit (realize m a) (realize m b)
   realize0 m (Bool_Ite p a b) = __caseTrue (realize m p) (realize m a) (realize m b)
   realize0 m (Bool_Prim r _) = r m

   cases0 p@True = concrete p
   cases0 p@False = concrete p
   cases0 p = switch p (concrete True) (concrete False)

   primitive0 = Bool_Prim
   error0 = Bool_Error

   __caseTrue0 x y n =
      case x of
        True -> y
        False -> n
        Bool_Error msg -> error0 msg
        _ -> Bool_Ite x y n

instance Haskelly Bool Bool where
  frhs = id
  stohs = id

instance Haskelly P.Bool Bool where
  frhs p = if p then True else False

  mtohs False = return P.False
  mtohs True = return P.True 
  mtohs _ = Nothing

  stohs False = P.False
  stohs True = P.True
  stohs _ = error "Bool stohs failed"


instance SmtenHS0 Integer where
   realize0 m x = 
      case x of
         Integer {} -> x
         Integer_Error {} -> x
         Integer_Add a b -> add_Integer (realize m a) (realize m b)
         Integer_Sub a b -> sub_Integer (realize m a) (realize m b)
         Integer_Ite p a b -> __caseTrue (realize m p) (realize m a) (realize m b)
         Integer_Var v -> frhs (as_lookup v m :: P.Integer)
         Integer_Prim r _ -> r m

   cases0 x = 
      case x of
        Integer {} -> Concrete x
        Integer_Ite p a b -> switch p (cases0 a) (cases0 b)
        Integer_Prim _ c -> c
        _ -> error "TODO: cases0 for symbolic Integer"

   primitive0 = Integer_Prim
   error0 = Integer_Error

   __caseTrue0 x y n =
      case x of
        True -> y
        False -> n
        Bool_Error msg -> error0 msg
        _ -> Integer_Ite x y n

instance Haskelly Integer Integer where
   frhs = id
   stohs = id

instance Haskelly P.Integer Integer where
   frhs = Integer

   mtohs (Integer x) = return x
   mtohs _ = Nothing

   stohs (Integer x) = x
   stohs _ = error "tohs.Integer failed"

eq_Integer :: Integer -> Integer -> Bool
eq_Integer = sprim2 ((==) :: P.Integer -> P.Integer -> P.Bool) Bool_EqInteger

leq_Integer :: Integer -> Integer -> Bool
leq_Integer = sprim2 ((<=) :: P.Integer -> P.Integer -> P.Bool) Bool_LeqInteger

add_Integer :: Integer -> Integer -> Integer
add_Integer = sprim2 ((+) :: P.Integer -> P.Integer -> P.Integer) Integer_Add

sub_Integer :: Integer -> Integer -> Integer
sub_Integer = sprim2 ((-) :: P.Integer -> P.Integer -> P.Integer) Integer_Sub

instance SmtenHS1 Bit where
   realize1 m c = 
      case c of
         Bit {} -> c
         Bit_Error {} -> c
         Bit_Add a b -> add_Bit (realize m a) (realize m b)
         Bit_Sub a b -> sub_Bit (realize m a) (realize m b)
         Bit_Mul a b -> mul_Bit (realize m a) (realize m b)
         Bit_Or a b -> or_Bit (realize m a) (realize m b)
         Bit_And a b -> and_Bit (realize m a) (realize m b)
         Bit_Shl a b -> shl_Bit (realize m a) (realize m b)
         Bit_Lshr a b -> lshr_Bit (realize m a) (realize m b)
         Bit_Concat a b -> concat_Bit (realize m a) (realize m b)
         Bit_Extract a b -> extract_Bit (realize m a) (realize m b)
         Bit_Not a -> not_Bit (realize m a)
         Bit_SignExtend a -> sign_extend_Bit (realize m a)
         Bit_Ite p a b -> __caseTrue (realize m p) (realize m a) (realize m b)
         Bit_Var x -> frhs (as_lookup x m :: P.Bit)
         Bit_Prim r _ -> r m
    
   cases1 x = 
      case x of
         Bit {} -> Concrete x
         Bit_Ite p a b -> switch p (cases0 a) (cases0 b)
         Bit_Prim _ c -> c
         _ -> error "TODO: cases0 for symbolic bit vector"
       
   primitive1 = Bit_Prim
   error1 = Bit_Error

   __caseTrue1 x y n =
      case x of
        True -> y
        False -> n
        Bool_Error msg -> error0 msg
        _ -> Bit_Ite x y n

instance Haskelly (Bit n) (Bit n) where
   frhs = id
   stohs = id

instance Haskelly P.Bit (Bit n) where
   frhs = Bit

   mtohs (Bit c) = return c
   mtohs _ = Nothing

   stohs (Bit c) = c
   stohs _ = error "tohs.Integer failed"

eq_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bool
eq_Bit = sprim2 ((==) :: P.Bit -> P.Bit -> P.Bool) Bool_EqBit

leq_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bool
leq_Bit = sprim2 ((<=) :: P.Bit -> P.Bit -> P.Bool) Bool_LeqBit

add_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
add_Bit = sprim2 ((+) :: P.Bit -> P.Bit -> P.Bit) Bit_Add

sub_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
sub_Bit = sprim2 ((-) :: P.Bit -> P.Bit -> P.Bit) Bit_Sub

mul_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
mul_Bit = sprim2 ((*) :: P.Bit -> P.Bit -> P.Bit) Bit_Mul

or_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
or_Bit = sprim2 ((.|.) :: P.Bit -> P.Bit -> P.Bit) Bit_Or

and_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
and_Bit = sprim2 ((.&.) :: P.Bit -> P.Bit -> P.Bit) Bit_And

shl_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
shl_Bit = sprim2 P.bv_shl Bit_Shl

lshr_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
lshr_Bit = sprim2 P.bv_lshr Bit_Lshr

not_Bit :: (SmtenHS0 n) => Bit n -> Bit n
not_Bit = sprim1 (complement :: P.Bit -> P.Bit) Bit_Not

sign_extend_Bit :: forall n m . (SmtenHS0 n, SmtenHS0 m) => Bit n -> Bit m
sign_extend_Bit =
  let w = (valueof0 (numeric :: m) - valueof0 (numeric :: n))
  in sprim1 (P.bv_sign_extend w) Bit_SignExtend

concat_Bit :: (SmtenHS0 n, SmtenHS0 m) => Bit n -> Bit m -> Bit npm
concat_Bit = sprim2 P.bv_concat Bit_Concat

extract_Bit :: forall n m . (SmtenHS0 n, SmtenHS0 m) => Bit n -> Integer -> Bit m
extract_Bit (Bit x) (Integer i) = 
  let hi = i + valueof0 (numeric :: m) - 1
      lo = i
  in frhs (P.bv_extract hi lo x)
extract_Bit b i = Bit_Extract b i

toInteger_Bit :: (SmtenHS0 n) => Bit n -> Integer
toInteger_Bit (Bit a) = frhs $ P.bv_value a
toInteger_Bit b = prim1 toInteger_Bit b

