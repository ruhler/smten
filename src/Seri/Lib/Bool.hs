
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Bool where

import Seri.Lambda.IR

trueE :: Exp
trueE = ConE (Sig "True" (ConT "Bool"))

falseE :: Exp
falseE = ConE (Sig "False" (ConT "Bool"))

