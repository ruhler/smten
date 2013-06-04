
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Smten.Runtime.SmtenHS where

import Prelude hiding (Bool(..), Integer)
import qualified Prelude

import Data.Dynamic
import Data.Functor((<$>))
import Data.Maybe(fromMaybe)

import Smten.Runtime.Haskelly
import Smten.SMT.FreeID
import Smten.CodeGen.TH

data Bool =
    False
  | True
  | BoolVar FreeID
  | BoolMux Bool Bool Bool
  | Bool__EqInteger Integer Integer

instance Haskelly Bool Bool where
  frhs = id
  tohs = return

instance Haskelly Prelude.Bool Bool where
  frhs p = if p then True else False
  tohs False = return Prelude.False
  tohs True = return Prelude.True 
  tohs _ = Nothing

data Integer =
    Integer Prelude.Integer
  | Integer_Add Integer Integer
  | IntegerMux__ Bool Integer Integer
  | IntegerVar FreeID

instance Haskelly Integer Integer where
   frhs = id
   tohs = return

instance Haskelly Prelude.Integer Integer where
   frhs = Integer
   tohs (Integer c) = return c
   tohs _ = Nothing



-- mux :: R.Bool -> a -> a -> a
-- mux p x y = if p then x else y
-- Except here p, x, and y may all be symbolic.
-- You may assume the predicate p is symbolic (you don't have to check for
-- it being true or false. That's taken care of elsewhere).

-- realize :: [(FreeID, R.Bool)] -> a -> a
-- Update all variables in the given expression according to the given map.
-- You may assume all free variables in the expression are in the map.
declare_SmtenHS 0
declare_SmtenHS 1
declare_SmtenHS 2
declare_SmtenHS 3
declare_SmtenHS 4

derive_SmtenHS 0
derive_SmtenHS 1
derive_SmtenHS 2
derive_SmtenHS 3

instance SmtenHS0 Bool where
   mux0 = BoolMux
   realize0 m True = True
   realize0 m False = False
   realize0 m (BoolVar x) = fromMaybe (error "realize0 Bool failed") $ do
      d <- lookup x m
      frhs <$> (fromDynamic d :: Maybe Prelude.Bool)
   realize0 m (BoolMux p a b)
      = __caseTrue (realize0 m p) (realize0 m a) (realize0 m b)
   realize0 m (Bool__EqInteger a b) = eq_Integer (realize0 m a) (realize0 m b)

instance SmtenHS0 Integer where
   mux0 = IntegerMux__
   realize0 m c = 
      case c of
         Integer {} -> c
         Integer_Add a b -> add_Integer (realize0 m a) (realize0 m b)
         IntegerMux__ p a b -> __caseTrue (realize0 m p) (realize0 m a) (realize0 m b)
         IntegerVar x -> fromMaybe (error "realize0 Integer failed") $ do
            d <- lookup x m
            frhs <$> (fromDynamic d :: Maybe Prelude.Integer)

instance SmtenHS1 Poly where
   mux1 p (Poly a) (Poly b) = Poly (mux0 p a b)
   realize1 m (Poly a) = Poly (realize0 m a)

instance SmtenHS2 (->) where
   mux2 p fa fb = \x -> mux0 p (fa x) (fb x)
   realize2 m f = \x -> realize0 m (f x)

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

eq_Integer :: Integer -> Integer -> Bool
eq_Integer (Integer a) (Integer b) = frhs (a == b)
eq_Integer a b = Bool__EqInteger a b

add_Integer :: Integer -> Integer -> Integer
add_Integer (Integer a) (Integer b) = Integer (a+b)
add_Integer a b = Integer_Add a b

