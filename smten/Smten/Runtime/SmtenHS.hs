
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smten.Runtime.SmtenHS where

import Prelude hiding (Bool(..), Integer)
import qualified Prelude as P
import qualified Smten.Bit as P

import Data.Bits
import Data.Dynamic
import Data.Functor((<$>))
import Data.Maybe(fromMaybe)

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

data Integer =
    Integer P.Integer
  | Integer_Add Integer Integer
  | Integer_Sub Integer Integer
  | Integer_Ite Bool Integer Integer
  | Integer_Var FreeID
  | Integer_Prim (Assignment -> Integer) (Cases Integer)

data Bit n =
    Bit P.Bit
  | Bit_Add (Bit n) (Bit n)
  | Bit_Sub (Bit n) (Bit n)
  | Bit_Mul (Bit n) (Bit n)
  | Bit_Or (Bit n) (Bit n)
  | Bit_Ite Bool (Bit n) (Bit n)
  | Bit_Var FreeID
  | Bit_Prim (Assignment -> (Bit n)) (Cases (Bit n))
  

class SmtenHS0 a where
    -- Update all variables in the given expression according to the given map.
    realize0 :: Assignment -> a -> a

    -- Return the cases of 'a'.
    cases0 :: a -> Cases a

    -- Represent a primitive function resulting in the given object.
    primitive0 :: (Assignment -> a) -> Cases a -> a

    __caseTrue0 :: Bool -> a -> a -> a
    __caseTrue0 x y n =
        case x of
            True -> y
            False -> n
            _ -> primitive0 (\m -> __caseTrue0 (realize0 m x) (realize0 m y) (realize0 m n))
                            (switch x (cases0 y) (cases0 n))


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
prim1 f x = primitive0 (\m -> f (realize0 m x)) (fmap f (cases0 x))

prim3 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c, SmtenHS0 d) => (a -> b -> c -> d) -> a -> b -> c -> d
prim3 f x y z = primitive0 (\m -> f (realize0 m x) (realize0 m y) (realize0 m z))
                           (f3map f (cases0 x) (cases0 y) (cases0 z))

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
   realize2 m f = \x -> realize0 m (f x)
   cases2 f = concrete f
   primitive2 r c = \x -> primitive0 (\m -> r m $ realize0 m x) (fmap ($ x) c)

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
   realize0 m True = True
   realize0 m False = False
   realize0 m (Bool_Var x) = fromMaybe (error "realize0.Bool") $ do
      v <- lookup x m
      frhs <$> (fromDynamic v :: Maybe P.Bool)
   realize0 m (Bool_EqInteger a b) = eq_Integer (realize0 m a) (realize0 m b)
   realize0 m (Bool_LeqInteger a b) = leq_Integer (realize0 m a) (realize0 m b)
   realize0 m (Bool_EqBit a b) = eq_Bit (realize0 m a) (realize0 m b)
   realize0 m (Bool_LeqBit a b) = leq_Bit (realize0 m a) (realize0 m b)
   realize0 m (Bool_Ite p a b) = __caseTrue (realize0 m p) (realize0 m a) (realize0 m b)
   realize0 m (Bool_Prim r _) = r m

   cases0 True = concrete True
   cases0 False = concrete False
   cases0 p = switch p (concrete True) (concrete False)

   primitive0 = Bool_Prim

   __caseTrue0 x y n =
      case x of
        True -> y
        False -> n
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
         Integer_Add a b -> add_Integer (realize0 m a) (realize0 m b)
         Integer_Sub a b -> sub_Integer (realize0 m a) (realize0 m b)
         Integer_Ite p a b -> __caseTrue (realize0 m p) (realize0 m a) (realize0 m b)
         Integer_Var v -> fromMaybe (error "realize0 Integer failed") $ do
            d <- lookup v m
            frhs <$> (fromDynamic d :: Maybe P.Integer)
         Integer_Prim r _ -> r m

   cases0 x = 
      case x of
        Integer {} -> Concrete x
        Integer_Prim _ c -> c
        _ -> error "TODO: cases0 for symbolic Integer"

   primitive0 = Integer_Prim

   __caseTrue0 x y n =
      case x of
        True -> y
        False -> n
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
eq_Integer (Integer a) (Integer b) = frhs (a == b)
eq_Integer a b = Bool_EqInteger a b

leq_Integer :: Integer -> Integer -> Bool
leq_Integer (Integer a) (Integer b) = frhs (a <= b)
leq_Integer a b = Bool_LeqInteger a b

add_Integer :: Integer -> Integer -> Integer
add_Integer (Integer a) (Integer b) = Integer (a+b)
add_Integer a b = Integer_Add a b

sub_Integer :: Integer -> Integer -> Integer
sub_Integer (Integer a) (Integer b) = Integer (a-b)
sub_Integer a b = Integer_Sub a b


instance SmtenHS1 Bit where
   realize1 m c = 
      case c of
         Bit {} -> c
         Bit_Add a b -> add_Bit (realize0 m a) (realize0 m b)
         Bit_Sub a b -> sub_Bit (realize0 m a) (realize0 m b)
         Bit_Mul a b -> mul_Bit (realize0 m a) (realize0 m b)
         Bit_Or a b -> or_Bit (realize0 m a) (realize0 m b)
         Bit_Ite p a b -> __caseTrue (realize0 m p) (realize0 m a) (realize0 m b)
         Bit_Var x -> fromMaybe (error "realize0 Bit failed") $ do
            d <- lookup x m
            frhs <$> (fromDynamic d :: Maybe P.Bit)
         Bit_Prim r _ -> r m
    
   cases1 x = 
      case x of
         Bit {} -> Concrete x
         Bit_Prim _ c -> c
         _ -> error "TODO: cases0 for symbolic bit vector"
       
   primitive1 = Bit_Prim

   __caseTrue1 x y n =
      case x of
        True -> y
        False -> n
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
eq_Bit (Bit a) (Bit b) = frhs (a == b)
eq_Bit a b = Bool_EqBit a b

leq_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bool
leq_Bit (Bit a) (Bit b) = frhs (a <= b)
leq_Bit a b = Bool_LeqBit a b

add_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
add_Bit (Bit a) (Bit b) = Bit (a+b)
add_Bit a b = Bit_Add a b

sub_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
sub_Bit (Bit a) (Bit b) = Bit (a+b)
sub_Bit a b = Bit_Sub a b

mul_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
mul_Bit (Bit a) (Bit b) = Bit (a+b)
mul_Bit a b = Bit_Mul a b

or_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
or_Bit (Bit a) (Bit b) = Bit (a .|. b)
or_Bit a b = Bit_Or a b

toInteger_Bit :: (SmtenHS0 n) => Bit n -> Integer
toInteger_Bit (Bit a) = frhs $ P.bv_value a
toInteger_Bit b = prim1 toInteger_Bit b

