
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Unit where

import Seri.IR
import Seri.Typed
import Seri.Declarations

declprim "()" [t| Typed Exp () |]
instance SeriType () where
    seritype _ = ConT "()"


