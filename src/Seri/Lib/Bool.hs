
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Bool where

import Seri.IR
import Seri.Typed
import Seri.Builtin
import Seri.Declarations
import Seri.Ppr
import Seri.Quoter
import Seri.Lib.Tuple

trueE :: Exp
trueE = ConE (ConT "Bool") "True"

falseE :: Exp
falseE = ConE (ConT "Bool") "False"

decltype ''Bool

[s|
    (&&) :: Bool -> Bool -> Bool
    (&&) True True = True
    (&&) _ _ = False
|]

