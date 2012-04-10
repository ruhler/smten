
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Primitives where

import Seri.IR
import Seri.Typed
import Seri.Declarations

declprim "unit" [t| Typed Exp () |]

