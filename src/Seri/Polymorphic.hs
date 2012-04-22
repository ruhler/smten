
{-# LANGUAGE TemplateHaskell #-}

module Seri.Polymorphic where

import Seri.Declarations.Polymorphic
import Seri.Declarations.User
import Seri.Typed

-- Dummy haskell types corresponding to variable types in seri.
-- Lets us express polymorphic seri expressions with a concrete haskell type.
decltyvars tyvars

