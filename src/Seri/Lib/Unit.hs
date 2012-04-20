
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Unit where

import Seri

decltycon 0 ''()
declcon "()" [t| () |]

