
module Smten.Prim (
    smtenPs,
    module Smten.Prim.Prim,
    module Smten.Prim.Prelude,
    module Smten.Prim.Bit,
    ) where

import Smten.Prim.Prim
import Smten.Prim.Prelude
import Smten.Prim.Bit

smtenPs :: [Prim]
smtenPs = concat [preludePs, bitPs]

