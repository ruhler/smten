
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Unit where

import Seri

declcon "()" [t| () |]
instance SeriType () where
    seritype _ = ConT "()"

