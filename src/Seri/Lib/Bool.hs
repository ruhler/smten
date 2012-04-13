
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Bool where

import Seri.IR
import Seri.Typed
import Seri.Declarations

trueE :: Exp
trueE = ConE (ConT "Bool") "True"

falseE :: Exp
falseE = ConE (ConT "Bool") "False"

declval "True" [t| Bool |] [e| conE "True" |] []
declval "False" [t| Bool |] [e| conE "False" |] []

instance SeriType Bool where
    seritype _ = ConT "Bool"

