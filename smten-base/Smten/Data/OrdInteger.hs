
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternGuards #-}
module Smten.Data.OrdInteger () where

import Smten.Base.GHC.Classes
import Smten.GHC.Integer.Type
import Smten.Data.EqInteger ()

instance Ord Integer where
    (<=) = leInteger

