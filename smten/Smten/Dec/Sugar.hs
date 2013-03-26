
module Smten.Dec.Sugar (nodefault) where

import Smten.Ppr
import Smten.Sig
import Smten.Exp
import Smten.Dec.Dec

-- Create a class method with no default body.
nodefault :: TopSig -> TopExp
nodefault s@(TopSig n _ t) =
  let msg = "no default implementation for " ++ pretty (Sig n t)
  in TopExp s (errorE msg)

