
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Bool (
    Bool, __True, __False,
    ) where

import Smten.Runtime.Formula

type Bool = BoolF

__True :: Bool
__True = trueF

__False :: Bool
__False = falseF


