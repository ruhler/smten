
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}

module Seri.Elaborate.ExpH (
      transform,
      de_appv1, de_appv2,

  ) where

import Data.Monoid

import Seri.Bit
import Seri.Name
import Seri.Sig
import Seri.Type
import Seri.ExpH

-- Perform a generic transformation on an expression.
-- Applies the given function to each subexpression. Any matching
-- subexpression is replaced with the returned value, otherwise it continues
-- to recurse.
transform :: (ExpH -> Maybe ExpH) -> ExpH -> ExpH
transform g e | Just v <- g e = v
transform g e 
  = case e of
       LitEH {} -> e
       ConEH {} -> e
       VarEH {} -> e 
       AppEH _ f x -> AppEH ES_None (transform g f) (transform g x)
       LamEH _ s f -> LamEH ES_None s $ \x -> transform g (f x)
       CaseEH _ x k y d -> CaseEH ES_None (transform g x) k (transform g y) (transform g d)

-- Match an application of the variable with given name to a single argument.
-- Returns the argument.
de_appv1 :: Name -> ExpH -> Maybe ExpH
de_appv1 n e 
    | (v, [x]) <- de_appsEH e
    , Just (Sig nm _) <- de_varEH v
    , n == nm
    = Just x
de_appv1 _ _ = Nothing

de_appv2 :: Name -> ExpH -> Maybe (ExpH, ExpH)
de_appv2 n e
    | (v, [x, y]) <- de_appsEH e
    , Just (Sig nm _) <- de_varEH v
    , n == nm
    = Just (x, y)
de_appv2 _ _ = Nothing

