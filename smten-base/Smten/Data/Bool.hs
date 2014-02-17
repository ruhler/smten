
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O #-}

module Smten.Data.Bool (
    Bool(..), (&&), (||), not, otherwise,
  ) where

import GHC.Types (Bool(..))
import GHC.Classes ((&&), (||), not)

otherwise :: Bool
otherwise = True

