
{-# LANGUAGE TemplateHaskell #-}

module Seri.DSEL.SMT (
    Query, Answer(..), RunOptions(..), runQuery,
    free, assert, Seri.DSEL.SMT.realize, run, run',
    queryR, query,
 ) where

import Data.Functor
import Data.Maybe(fromJust)

import Seri.SMT.Query hiding (free, assert, query)
import Seri.SMT.Solver (Solver)
import qualified Seri.SMT.Query as Q
import qualified Seri.SMT.Run as Q
import Seri.DSEL.DSEL
import Seri.Elaborate hiding (query)
import Seri.Type.SeriT
import Seri.Type.TH
import Seri.ExpH.SeriEH

-- Dummy query type with instance of SeriableT.
data QueryT a = QueryT
derive_SeriT ''QueryT

free :: (Solver s, SeriT a) => Query s (ExpT a)
free = 
    let freeE :: (SeriT a) => ExpT (QueryT a)
        freeE = varET "Seri.SMT.SMT.free"
    in run freeE

assert :: (Solver s) => ExpT Bool -> Query s ()
assert p =
  let assertE :: ExpT (Bool -> QueryT ())
      assertE = varET "Seri.SMT.SMT.assert"
  in run' (apply assertE p)

realize :: (Solver s, SeriEH a) => ExpT a -> Realize s a
realize (ExpT x) = do
  env <- envR
  fromJust . de_seriEH . elabwhnf env <$> Q.realize x

queryR :: (Solver s) => Realize s a -> Query s (Answer a)
queryR = Q.query

query :: (Solver s, SeriEH a) => ExpT a -> Query s (Answer a)
query = queryR . Seri.DSEL.SMT.realize

run :: (Solver s) => ExpT (QueryT a) -> Query s (ExpT a)
run (ExpT x) = ExpT <$> Q.run x

run' :: (Solver s, SeriEH a) => ExpT (QueryT a) -> Query s a
run' x = do
  env <- envQ
  ExpT v <- run x
  return . fromJust . de_seriEH $ elabwhnf env v

