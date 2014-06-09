
{-# LANGUAGE NoImplicitPrelude #-}

module Smten.GHC.Integer.EqOrd () where

import Smten.Base.GHC.Classes
import Smten.GHC.Integer.Type

instance Eq Integer where
    (==) = eqInteger

instance Ord Integer where
    (<=) = leInteger

