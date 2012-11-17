
{-# LANGUAGE TemplateHaskell #-}

module Seri.DSEL.SMT (
    Query, Answer(..), RunOptions(..), runQuery,
    free, assert, Seri.DSEL.SMT.realize, run, run',
    queryR, query,
 ) where

import Data.Functor
import Data.Maybe(fromJust)

import Seri.SMT.Query hiding (free, assert, query)
import qualified Seri.SMT.Query as Q
import qualified Seri.SMT.Run as Q
import Seri.DSEL.DSEL
import Seri.Elaborate hiding (query)
import Seri.Type
import Seri.ExpH

-- Dummy query type with instance of SeriableT.
data QueryT a = QueryT
derive_SeriT ''QueryT

free :: (SeriT a) => Query (ExpT a)
free = 
    let freeE :: (SeriT a) => ExpT (QueryT a)
        freeE = varET "Seri.SMT.SMT.free"
    in run freeE

assert :: ExpT Bool -> Query ()
assert p =
  let assertE :: ExpT (Bool -> QueryT ())
      assertE = varET "Seri.SMT.SMT.assert"
  in run' (apply assertE p)

realize :: (SeriEH a) => ExpT a -> Realize a
realize (ExpT x) = do
  env <- envR
  fromJust . de_seriEH . elabwhnf env <$> Q.realize x

queryR :: Realize a -> Query (Answer a)
queryR = Q.query

query :: (SeriEH a) => ExpT a -> Query (Answer a)
query = queryR . Seri.DSEL.SMT.realize

run :: ExpT (QueryT a) -> Query (ExpT a)
run (ExpT x) = ExpT <$> Q.run x

run' :: (SeriEH a) => ExpT (QueryT a) -> Query a
run' x = do
  env <- envQ
  ExpT v <- run x
  return . fromJust . de_seriEH $ elabwhnf env v

