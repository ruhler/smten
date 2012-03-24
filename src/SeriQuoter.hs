
module SeriQuoter (s)
    where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import SeriTHLift
import SeriParser

s :: QuasiQuoter 
s = QuasiQuoter qexp qpat qtype qdec

qexp :: String -> Q Exp
qexp str = seriparse str >>= lift

qpat :: String -> Q Pat
qpat = error $ "Seri pattern quasi-quote not supported"

qtype :: String -> Q Type
qtype = error $ "Seri type quasi-quote not supported"

qdec :: String -> Q [Dec]
qdec = error $ "Seri dec quasi-quote not supported"

