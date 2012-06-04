
{-# LANGUAGE TemplateHaskell #-}

module Seri.FrontEnd.Polymorphic where

import Seri.FrontEnd.Declarations.Polymorphic
import Seri.FrontEnd.Declarations.User
import Seri.FrontEnd.Typed

-- Dummy haskell types corresponding to variable types in seri.
-- Lets us express polymorphic seri expressions with a concrete haskell type.
decltyvars tyvars

