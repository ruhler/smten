
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.State (tests) where

import Smten.Prelude
import Smten.Control.Monad.State
import Smten.Tests.Test

-- Test an ambiguous type which can come from using StateT.
fambig :: (Eq t) => (t, t) -> Bool
fambig (a, b) = a == b

tambig :: forall t . (Eq t) => State (t, t) Bool
tambig = do
    x <- get                        -- ambiguous type for 'get'
    return (fambig (x :: (t, t)))   -- use scoped type variables to fix.

funambig :: (t, t) -> State (t, t) ()
funambig _ = return ()

tunambig :: (Eq t) => State (t, t) Bool
tunambig = do
    x <- get
    funambig x
    return (fambig x)

statething :: Integer -> State Integer Integer
statething x = do
    s <- get
    put (s+x)
    return (s*x)

-- StateT of IO should be an instance of MonadIO.
stateio :: StateT Integer IO Integer
stateio = do
    liftIO $ putStrLn "hello!"
    return 5

tests :: IO ()
tests = do
    test "State" (runState (statething 2) 3 == (6, 5))

    putStrLn "State PASSED"

