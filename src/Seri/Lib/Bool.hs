
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Bool where

import qualified Seri.Lambda.Bool
import Seri.Lambda.IR
import Seri.FrontEnd.Typed
import Seri.FrontEnd.Declarations.User
import Seri.FrontEnd.Quoter
import Seri.Utils.Ppr
import Seri.Lib.Tuple

trueE :: Exp
trueE = Seri.Lambda.Bool.trueE

falseE :: Exp
falseE = Seri.Lambda.Bool.falseE

decltycon 0 ''Bool
declcon "True" [t| Bool |]
declcon "False" [t| Bool |]

[s|
    (&&) :: Bool -> Bool -> Bool
    (&&) True True = True
    (&&) _ _ = False
|]

