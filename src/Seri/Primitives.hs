
{-# LANGUAGE TemplateHaskell #-}

module Seri.Primitives (
        _seriP_unit, _seriC_unit, _seriD_unit,
        _seriP_True, _seriC_True, _seriD_True,
        _seriP_False, _seriC_False, _seriD_False,
        _seriP_fix, _seriC_fix, _seriD_fix,

 ) where

import Seri.IR
import Seri.Typed
import Seri.Declarations

declprim "unit" 'UnitP [t| TypedExp () |]
declprim "True" 'TrueP [t| TypedExp Bool |]
declprim "False" 'FalseP [t| TypedExp Bool |]
declprim "fix" 'FixP [t| (SeriType a) => TypedExp ((a -> a) -> a) |]
 
