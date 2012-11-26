
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

