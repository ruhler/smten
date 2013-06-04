
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Smten.Runtime.SmtenHS where

import Prelude hiding (Bool(..))
import qualified Prelude

import Smten.Runtime.Haskelly
import Smten.SMT.FreeID
import Smten.CodeGen.TH

data Bool =
    False
  | True
  | BoolVar FreeID
  | BoolMux Bool Bool Bool

instance Haskelly Bool Bool where
  frhs = id
  tohs = return

instance Haskelly Prelude.Bool Bool where
  frhs p = if p then True else False
  tohs False = return Prelude.False
  tohs True = return Prelude.True 
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
   realize0 m (BoolVar x) = 
      case lookup x m of
          Just Prelude.True -> True
          Just Prelude.False -> False
          Nothing -> error $ "realize0 Bool failed"
   realize0 m (BoolMux p a b)
      = __caseTrue (realize0 m p) (realize0 m a) (realize0 m b)

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


