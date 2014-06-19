
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
module Smten.Compiled.Smten.Debug.Trace0 (trace, traceS) where

import qualified Prelude as P
import qualified Debug.Trace as P

import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf
import Smten.Compiled.Smten.Smten.Base

trace :: (SmtenHS0 a) => List__ Char -> a -> a
trace = symapp P.trace

traceS :: (SmtenHS0 a) => a -> b -> b
traceS x y = P.trace (traceS0 x) y

