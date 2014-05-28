
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Show (
    ShowS, Show(..), shows, showChar, showString, showParen,
    ) where

-- Expose the Prelude version of Show, but also import our local
-- version so code is generated for it.
import Prelude

