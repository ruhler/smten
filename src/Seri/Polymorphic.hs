
{-# LANGUAGE TemplateHaskell #-}

module Seri.Polymorphic where

import Seri.Declarations.User
import Seri.Typed

-- Dummy haskell types corresponding to variable types in seri.
-- Lets us express polymorphic seri expressions with a concrete haskell type.
-- Note: This needs to be kept in sync with tvarkind in
-- Seri.Declarations.Utils

-- Some of kind *
decltyvar 0 "a"
decltyvar 0 "b"
decltyvar 0 "c"
decltyvar 0 "d"

-- Some of kind * -> *.
decltyvar 1 "m"

