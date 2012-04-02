
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

declprim "unit" 'UnitP [t| Typed Exp () |]
declprim "True" 'TrueP [t| Typed Exp Bool |]
declprim "False" 'FalseP [t| Typed Exp Bool |]
declprim "fix" 'FixP [t| (SeriType a) => Typed Exp ((a -> a) -> a) |]
 
