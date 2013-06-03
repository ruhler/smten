
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Smten.Runtime.SmtenHS where

import qualified Smten.Runtime.Prelude as R
import Smten.Runtime.Haskelly
import Smten.SMT.FreeID
import Smten.CodeGen.TH

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

instance SmtenHS0 R.Bool where
   mux0 = error "TODO: mux0 Bool"
   realize0 m b = b

instance SmtenHS1 Poly where
   mux1 p (Poly a) (Poly b) = Poly (mux0 p a b)
   realize1 m (Poly a) = Poly (realize0 m a)

instance SmtenHS2 (->) where
   mux2 p fa fb = \x -> mux0 p (fa x) (fb x)
   realize2 m f = \x -> realize0 m (f x)
