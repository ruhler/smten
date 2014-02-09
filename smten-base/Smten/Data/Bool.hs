
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O #-}

module Smten.Data.Bool (
    Bool(..), (&&), (||), not, otherwise,
  ) where

import Prelude ((&&), (||), not)
import Smten.Data.Bool0

otherwise :: Bool
otherwise = True

