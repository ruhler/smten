
{-# LANGUAGE TemplateHaskell #-}

module Seri.Quoter (s)
    where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Lift(..))
import qualified Language.Haskell.TH as TH

import Seri.Parser
import Seri.IR

s :: QuasiQuoter 
s = QuasiQuoter qexp qpat qtype qdec

qexp :: String -> TH.Q TH.Exp
qexp str = seriparse str >>= lift

qpat :: String -> TH.Q TH.Pat
qpat = error $ "Seri pattern quasi-quote not supported"

qtype :: String -> TH.Q TH.Type
qtype = error $ "Seri type quasi-quote not supported"

qdec :: String -> TH.Q [TH.Dec]
qdec = error $ "Seri dec quasi-quote not supported"


instance Lift Type where
    lift IntegerT = [e| IntegerT |]
    lift (ArrowT a b) = [e| ArrowT a b |]
    lift UnknownT = [e| UnknownT |]

instance Lift Exp where
    lift (IntegerE x) = [e| IntegerE x |]
    lift (AddE a b) = [e| AddE a b |]
    lift (MulE a b) = [e| MulE a b |]
    lift (AppE t a b) = [e| AppE t a b |]
    lift (LamE t n e) = [e| LamE t n e |]
    lift (VarE t n) = [e| VarE t n |]
    lift (ThE x) = TH.appE [e| seriate |] (return x)

