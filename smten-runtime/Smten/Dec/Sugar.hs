
module Smten.Dec.Sugar (nodefault) where

import Smten.Location
import Smten.Ppr
import Smten.Sig
import Smten.Exp
import Smten.Dec.Dec

-- Create a class method with no default body.
nodefault :: Location -> TopSig -> TopExp
nodefault l s@(TopSig n _ t) =
  let msg = "no default implementation for " ++ pretty (Sig n t)
  in TopExp s (errorE l (lmsg l msg))

