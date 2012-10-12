
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Enoch.SMT (
    Query, Answer(..), 
    free, assert, Seri.Enoch.SMT.realize, run, run',
    queryR, query,
 ) where

import Data.Functor

import Seri.Lambda hiding (free, query)
import Seri.SMT.Query hiding (free, assert, query)
import Seri.SMT.Solver (Solver)
import qualified Seri.SMT.Query as Q
import qualified Seri.SMT.Run as Q
import Seri.Enoch.Enoch
import Seri.Enoch.EnochTH
import Seri.Enoch.Prelude
import Seri.Target.Elaborate

derive_SeriableT ''Answer
derive_SeriableE ''Answer

-- Dummy query type with instance of SeriableT.
data QueryT a = QueryT
derive_SeriableT ''QueryT

free :: (Solver s, SeriableT a) => Query s (TExp a)
free = 
    let freeE :: (SeriableT a) => TExp (QueryT a)
        freeE = varE "Seri.SMT.SMT.free"
    in run freeE

assert :: (Solver s) => TExp Bool -> Query s ()
assert p =
  let assertE :: TExp (Bool -> QueryT ())
      assertE = varE "Seri.SMT.SMT.assert"
  in run' (apply assertE p)

realize :: (Solver s, SeriableE a) => TExp a -> Realize (Query s) a
realize (TExp x) = do
  env <- envR
  unpack' . TExp . elabwhnf env <$> Q.realize x

queryR :: (Solver s) => Realize (Query s) a -> Query s (Answer a)
queryR = Q.query

query :: (Solver s, SeriableE a) => TExp a -> Query s (Answer a)
query = queryR . Seri.Enoch.SMT.realize

run :: (Solver s) => TExp (QueryT a) -> Query s (TExp a)
run (TExp x) = TExp <$> Q.run x

run' :: (Solver s, SeriableE a) => TExp (QueryT a) -> Query s a
run' x = do
  env <- envQ
  TExp v <- run x
  return $ unpack' (TExp (elabwhnf env v))

