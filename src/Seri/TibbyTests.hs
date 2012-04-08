
module Seri.TibbyTests (tests) where

import Test.HUnit

import Seri
import Seri.Tibby

data Forth = Forth {
    request :: Put Integer
    response :: Put' Integer
}
declifc ''Forth

[s| 
    mkForth :: Forth' -> Module (Program ())
    mkForth = \ifc -> do
        (sq_g, sq_p) <- form
        return . loop $ seq [
            action $ do
                x <- get $ request' ifc
                put sq_p (x*x),
            action $ do
                squared <- get sq_g
                put (response' ifc) (squared * squared)
            ]
|]
