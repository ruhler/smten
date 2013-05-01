
module Smten.Prim (
    smtenPs,
    module Smten.Prim.Prim,
    module Smten.Prim.Prelude,
    module Smten.Prim.Bit,
    module Smten.Prim.Array,
    ) where

import Smten.Prim.Prim
import Smten.Prim.Prelude
import Smten.Prim.Bit
import Smten.Prim.Array

smtenPs :: [Prim]
smtenPs = concat [preludePs, bitPs, arrayPs]

