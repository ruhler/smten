
module Seri.Prim (
    seriPs,
    module Seri.Prim.Prim,
    module Seri.Prim.Prelude,
    module Seri.Prim.Bit,
    ) where

import Seri.Prim.Prim
import Seri.Prim.Prelude
import Seri.Prim.Bit

seriPs :: [Prim]
seriPs = concat [preludePs, bitPs]

