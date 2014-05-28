
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternGuards #-}
module Smten.Data.OrdInteger () where

import GHC.Integer (Integer)
import Smten.Data.EqInteger ()
import Smten.Base.GHC.Classes
import Smten.GHC.Integer.Type

instance Ord Integer where
    (<=) = leInteger

