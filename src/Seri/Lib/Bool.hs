
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Bool where

import Seri.IR
import Seri.Typed
import Seri.Builtin
import Seri.Declarations.User
import Seri.Ppr
import Seri.Quoter
import Seri.Lib.Tuple

trueE :: Exp
trueE = ConE (ConT "Bool") "True"

falseE :: Exp
falseE = ConE (ConT "Bool") "False"

declcon "True" [t| Bool |]
declcon "False" [t| Bool |]

instance SeriType Bool where
    seritype _ = ConT "Bool"

boolB :: Builtin
boolB =
  let mp _ = Nothing
      mt _ = Nothing
  in Builtin {
     mapprim = mp,
     maptype = mt,
     includes = text "import Data.Bool(Bool(..))"
  }


[s|
    (&&) :: Bool -> Bool -> Bool
    (&&) True True = True
    (&&) _ _ = False
|]

