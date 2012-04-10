
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Unit where

import Seri

declprim "()" [t| Typed Exp () |]
instance SeriType () where
    seritype _ = ConT "()"


