
module Smten.Data.Num (
    Num(..)
     ) where

-- Expose the Prelude version of Num, but also import our local
-- version so code is generated for it.
import Prelude
import Smten.GHC.Num ()

