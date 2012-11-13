
module Seri.Exp.Ppr () where

import Seri.Ppr
import Seri.Exp.Exp
import Seri.Exp.Sugar

instance Ppr Exp where
    ppr e = error $ "TODO: Exp.ppr " ++ show e

