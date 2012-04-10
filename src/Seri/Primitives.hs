
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Primitives where

import Seri.IR
import Seri.Typed
import Seri.Declarations

declprim "unit" [t| Typed Exp () |]

trueE :: Exp
trueE = ConE (ConT "Bool") "True"

falseE :: Exp
falseE = ConE (ConT "Bool") "False"

declval "True" [t| Typed Exp Bool |] [e| conE "True" |] []
declval "False" [t| Typed Exp Bool |] [e| conE "False" |] []


