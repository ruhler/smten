
module Seri.Lambda.Sugar (
    ifE
    ) where

import Seri.Lambda.IR

ifE :: Exp -> Exp -> Exp -> Exp
ifE p a b = CaseE p [Match (ConP (Sig "True" (ConT "Bool")) []) a,
                     Match (ConP (Sig "False" (ConT "Bool")) []) b]

