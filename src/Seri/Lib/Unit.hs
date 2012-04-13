
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Unit where

import Seri

declprim "()" [t| () |]
instance SeriType () where
    seritype _ = ConT "()"


