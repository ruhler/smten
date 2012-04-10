
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Bool where

import Seri.IR
import Seri.Typed
import Seri.Declarations

trueE :: Exp
trueE = ConE (ConT "Bool") "True"

falseE :: Exp
falseE = ConE (ConT "Bool") "False"

declval "True" [t| Typed Exp Bool |] [e| conE "True" |] []
declval "False" [t| Typed Exp Bool |] [e| conE "False" |] []

instance SeriType Bool where
    seritype _ = ConT "Bool"

