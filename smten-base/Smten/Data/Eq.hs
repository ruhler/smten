
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O #-}
module Smten.Data.Eq (Eq(..)) where

-- We use the prelude's definition of Eq so that we can use Haskell's
-- deriving, and avoid the need for RebindableSyntax which arises from
-- the Num class.
--
-- Smten.Data.Eq1 provides a local definition which we can compile
-- to Smten code and use. It's imported here to ensure it is compiled to
-- Smten code.
import Prelude(Eq(..))
import Smten.Data.Eq1 ()
import Smten.Data.EqInteger ()

