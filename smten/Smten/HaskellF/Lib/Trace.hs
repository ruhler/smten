
module Smten.HaskellF.Lib.Trace (
    trace, traceE,
    ) where

import Prelude hiding (String)

import Smten.Prim.Prelude
import Smten.HaskellF.HaskellF
import Smten.HaskellF.Lib.Prelude
    
trace :: (HaskellF a) => String -> a -> a
trace = primHF traceP

traceE :: (HaskellF a, HaskellF b) => a -> b -> b
traceE = primHF traceEP

