
module Seri.Lambda.Bool(trueE, falseE) where

import Seri.Lambda.IR

-- The Lambda core has to know how booleans are represented to handle if
-- statements properly in elaboration. That representation is captured here.

trueE :: Exp
trueE = ConE (ConT "Bool") "True"

falseE :: Exp
falseE = ConE (ConT "Bool") "False"

