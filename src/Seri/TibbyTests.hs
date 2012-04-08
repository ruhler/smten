
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
    mkForth ifc = do
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

[s|
    -- Return a program which calls put for each element in a list.
    puts :: [Integer] -> Put Integer -> Program ()
    puts [] p = seq []
    puts (x:xs) p = seq ((action (put p x)) : (puts xs p))

    -- Return a program which calls get N times, terminating on the last call.
    gets :: Integer -> Put (Terminating Integer) -> Put' Integer -> Program ()
    gets 0 _ _ = seq []
    gets n p g = seq (action $ do
        x <- put' g
        put p $ Terminating (n == 1) x) : gets (n-1) p g
        
    mkTestFourth :: Put (Terminating Integer) -> Module (Program ())
    mkTestFourth ifc = do
        fourth <- make mkForth
        return $ par (puts [1..5]) (gets 5)
|]

ttest :: [a] -> Typed Exp (Module (Put' Terminating a)) -> Assertion
ttest wnt top = error $ "TODO: ttest"

tests = "Tibby" ~: [
    "fourth" ~: ttest [1, 16, 81, 256, 625] [s| make mkTestFourth |]
    ]

