
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Bool where

import Seri.IR
import Seri.Typed
import Seri.Builtin
import Seri.Declarations
import Seri.Ppr

trueE :: Exp
trueE = ConE (ConT "Bool") "True"

falseE :: Exp
falseE = ConE (ConT "Bool") "False"

declval "True" [t| Bool |] [e| conE "True" |]
declval "False" [t| Bool |] [e| conE "False" |]

instance SeriType Bool where
    seritype _ = ConT "Bool"

boolB :: Builtin
boolB =
  let mp _ = Nothing

      mt "Bool" = Just "Bool.Bool"
      mt _ = Nothing
  in Builtin {
     mapprim = mp,
     maptype = mt,
     includes = text "import qualified Seri.Target.Haskell.Lib.Bool as Bool"
  }

