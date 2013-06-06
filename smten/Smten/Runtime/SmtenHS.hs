
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Smten.Runtime.SmtenHS where

import Prelude hiding (Bool(..), Integer)
import qualified Prelude as P
import qualified Smten.Bit as P

import Data.Dynamic
import Data.Functor((<$>))
import Data.Maybe(fromMaybe)

import Smten.SMT.FreeID
import Smten.CodeGen.TH

data Bool =
    False
  | True
  | BoolVar FreeID
  | BoolMux Bool Bool Bool
  | Bool__EqInteger Integer Integer
  | Bool__LeqInteger Integer Integer
  | Bool__EqBit Bit Bit
  | Bool__LeqBit Bit Bit

data Integer =
    Integer P.Integer
  | Integer_Add Integer Integer
  | Integer_Sub Integer Integer
  | IntegerMux Bool Integer Integer
  | IntegerVar FreeID

data Bit =
    Bit P.Bit
  | Bit_Add Bit Bit
  | Bit_Sub Bit Bit
  | Bit_Mul Bit Bit
  | BitMux Bool Bit Bit
  | BitVar FreeID

-- mux :: R.Bool -> a -> a -> a
-- mux p x y = if p then x else y
-- Except here p, x, and y may all be symbolic.
-- You may assume the predicate p is symbolic (you don't have to check for
-- it being true or false. That's taken care of elsewhere).

-- realize :: [(FreeID, Dynamic)] -> a -> a
-- Update all variables in the given expression according to the given map.
-- You may assume all free variables in the expression are in the map.

-- strict_app :: (a -> b) -> a -> b
-- Perform strict application of the function to the argument.
-- In particular:
--   The function will never see a Mux or Error constructor for the object.
declare_SmtenHS 0
declare_SmtenHS 1
declare_SmtenHS 2
declare_SmtenHS 3
declare_SmtenHS 4

derive_SmtenHS 0
derive_SmtenHS 1
derive_SmtenHS 2
derive_SmtenHS 3

class Haskelly h s where
    frhs :: h -> s
    tohs :: s -> h

newtype Poly a = Poly a

instance SmtenHS1 Poly where
   mux1 p (Poly a) (Poly b) = Poly (mux0 p a b)
   realize1 m (Poly a) = Poly (realize0 m a)
   strict_app1 f p = f p

instance Haskelly (Poly s) s where
    frhs (Poly x) = x
    tohs = Poly

instance SmtenHS2 (->) where
   mux2 p fa fb = \x -> mux0 p (fa x) (fb x)
   realize2 m f = \x -> realize0 m (f x)
   strict_app2 g f = g f

instance (Haskelly ha sa, Haskelly hb sb, SmtenHS0 sa, SmtenHS0 sb)
         => Haskelly (ha -> hb) (sa -> sb) where
    frhs hf sx = strict_app0 (frhs . hf . tohs) sx
    tohs sf hx = tohs $ sf (frhs hx)

__caseTrue :: (SmtenHS0 z) => Bool -> z -> z -> z
__caseTrue x y n = 
   case x of
      True -> y
      False -> n
      _ -> mux0 x y n

__caseFalse :: (SmtenHS0 z) => Bool -> z -> z -> z
__caseFalse x y n =
   case x of
     False -> y
     True -> n
     _ -> mux0 x n y

instance SmtenHS0 Bool where
   mux0 = BoolMux

   realize0 m True = True
   realize0 m False = False
   realize0 m (BoolVar x) = fromMaybe (error "realize0 Bool failed") $ do
      d <- lookup x m
      frhs <$> (fromDynamic d :: Maybe P.Bool)
   realize0 m (BoolMux p a b)
      = __caseTrue (realize0 m p) (realize0 m a) (realize0 m b)
   realize0 m (Bool__EqInteger a b) = eq_Integer (realize0 m a) (realize0 m b)
   realize0 m (Bool__LeqInteger a b) = leq_Integer (realize0 m a) (realize0 m b)
   realize0 m (Bool__EqBit a b) = eq_Bit (realize0 m a) (realize0 m b)
   realize0 m (Bool__LeqBit a b) = leq_Bit (realize0 m a) (realize0 m b)

   strict_app0 f (BoolMux p a b) = mux0 p (strict_app0 f a) (strict_app0 f b)
   strict_app0 f b = f b

instance Haskelly Bool Bool where
  frhs = id
  tohs = id

instance Haskelly P.Bool Bool where
  frhs p = if p then True else False

  tohs False = P.False
  tohs True = P.True 
  tohs _ = error "tohs.Bool failed"



instance SmtenHS0 Integer where
   mux0 = IntegerMux

   realize0 m c = 
      case c of
         Integer {} -> c
         Integer_Add a b -> add_Integer (realize0 m a) (realize0 m b)
         Integer_Add a b -> sub_Integer (realize0 m a) (realize0 m b)
         IntegerMux p a b -> __caseTrue (realize0 m p) (realize0 m a) (realize0 m b)
         IntegerVar x -> fromMaybe (error "realize0 Integer failed") $ do
            d <- lookup x m
            frhs <$> (fromDynamic d :: Maybe P.Integer)

   strict_app0 f (IntegerMux p a b) = mux0 p (strict_app0 f a) (strict_app0 f b)
   strict_app0 f i = f i

instance Haskelly Integer Integer where
   frhs = id
   tohs = id

instance Haskelly P.Integer Integer where
   frhs = Integer
   tohs (Integer c) = c
   tohs _ = error "tohs.Integer failed"

eq_Integer :: Integer -> Integer -> Bool
eq_Integer (Integer a) (Integer b) = frhs (a == b)
eq_Integer a b = Bool__EqInteger a b

leq_Integer :: Integer -> Integer -> Bool
leq_Integer (Integer a) (Integer b) = frhs (a <= b)
leq_Integer a b = Bool__LeqInteger a b

add_Integer :: Integer -> Integer -> Integer
add_Integer (Integer a) (Integer b) = Integer (a+b)
add_Integer a b = Integer_Add a b

sub_Integer :: Integer -> Integer -> Integer
sub_Integer (Integer a) (Integer b) = Integer (a-b)
sub_Integer a b = Integer_Sub a b


instance SmtenHS0 Bit where
   mux0 = BitMux

   realize0 m c = 
      case c of
         Bit {} -> c
         Bit_Add a b -> add_Bit (realize0 m a) (realize0 m b)
         Bit_Sub a b -> sub_Bit (realize0 m a) (realize0 m b)
         Bit_Mul a b -> mul_Bit (realize0 m a) (realize0 m b)
         BitMux p a b -> __caseTrue (realize0 m p) (realize0 m a) (realize0 m b)
         BitVar x -> fromMaybe (error "realize0 Bit failed") $ do
            d <- lookup x m
            frhs <$> (fromDynamic d :: Maybe P.Bit)

   strict_app0 f (BitMux p a b) = mux0 p (strict_app0 f a) (strict_app0 f b)
   strict_app0 f i = f i

instance Haskelly Bit Bit where
   frhs = id
   tohs = id

instance Haskelly P.Bit Bit where
   frhs = Bit
   tohs (Bit c) = c
   tohs _ = error "tohs.Integer failed"

eq_Bit :: Bit -> Bit -> Bool
eq_Bit (Bit a) (Bit b) = frhs (a == b)
eq_Bit a b = Bool__EqBit a b

leq_Bit :: Bit -> Bit -> Bool
leq_Bit (Bit a) (Bit b) = frhs (a <= b)
leq_Bit a b = Bool__LeqBit a b

add_Bit :: Bit -> Bit -> Bit
add_Bit (Bit a) (Bit b) = Bit (a+b)
add_Bit a b = Bit_Add a b

sub_Bit :: Bit -> Bit -> Bit
sub_Bit (Bit a) (Bit b) = Bit (a+b)
sub_Bit a b = Bit_Sub a b

mul_Bit :: Bit -> Bit -> Bit
mul_Bit (Bit a) (Bit b) = Bit (a+b)
mul_Bit a b = Bit_Mul a b

