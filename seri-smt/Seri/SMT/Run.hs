
{-# LANGUAGE PatternGuards #-}

-- | Run a Seri Exp of type Query in the haskell Query monad.
module Seri.SMT.Run (run) where

import Debug.Trace

import Data.Maybe

import Seri.Type
import Seri.ExpH
import Seri.Name
import Seri.Sig
import Seri.Ppr
import Seri.SMT.Query
import Seri.SMT.Primitives

import Seri.Elaborate

-- | Given a Seri expression of type Query a,
-- returns the Seri expression of type a which results from running the query.
run :: ExpH -> Query ExpH
run e = fromMaybe (error "Query.run") (de_queryEH e)

