
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Bool0 (Bool, __True, __False) where

import Smten.Runtime.Formula

type Bool = BoolF

__True :: Bool
__True = trueF

__False :: Bool
__False = falseF

