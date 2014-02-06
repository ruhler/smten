
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O #-}
module Smten.Data.EqInteger () where

import Smten.Smten.Base
import Smten.Data.Eq0
import Smten.Data.Eq1

instance Eq Integer where
    (==) = integer_eq

