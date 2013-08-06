
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.State (tests) where

import Smten.Prelude
import Smten.Control.Monad.State
import Smten.Tests.Test

-- Test an ambiguous type which can come from using StateT.
_fambig :: (Eq t) => (t, t) -> Bool
_fambig (a, b) = a == b

_tambig :: forall t . (Eq t) => State (t, t) Bool
_tambig = do
    x <- get                        -- ambiguous type for 'get'
    return (_fambig (x :: (t, t)))   -- use scoped type variables to fix.

_funambig :: (t, t) -> State (t, t) ()
_funambig _ = return ()

_tunambig :: (Eq t) => State (t, t) Bool
_tunambig = do
    x <- get
    _funambig x
    return (_fambig x)

statething :: Integer -> State Integer Integer
statething x = do
    s <- get
    put (s+x)
    return (s*x)

-- StateT of IO should be an instance of MonadIO.
_stateio :: StateT Integer IO Integer
_stateio = do
    liftIO $ putStrLn "hello!"
    return 5

tests :: IO ()
tests = do
    test "State" (runState (statething 2) 3 == (6, 5))

    putStrLn "State PASSED"

