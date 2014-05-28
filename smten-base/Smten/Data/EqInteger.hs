
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O #-}
module Smten.Data.EqInteger () where

import Prelude (Integer)
import Smten.Base.GHC.Classes
import Smten.GHC.Integer.Type

instance Eq Integer where
    (==) = eqInteger

