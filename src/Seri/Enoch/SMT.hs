
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
import Seri.SMT.Query hiding (free, assert, query, run)
import qualified Seri.SMT.Query as Q
import Seri.Enoch.Enoch
import Seri.Enoch.EnochTH
import Seri.Enoch.Prelude
import Seri.Target.Elaborate

derive_SeriableT ''Answer
derive_SeriableE ''Answer

-- Dummy query type with instance of SeriableT.
data QueryT a = QueryT
derive_SeriableT ''QueryT

free :: (Query q, SeriableT a) => q (TExp a)
free = 
    let freeE :: (SeriableT a) => TExp (QueryT a)
        freeE = varE "Seri.SMT.SMT.free"
    in run freeE

assert :: (Query q) => TExp Bool -> q ()
assert p =
  let assertE :: TExp (Bool -> QueryT ())
      assertE = varE "Seri.SMT.SMT.assert"
  in run' (apply assertE p)

realize :: (Query q, SeriableE a) => TExp a -> Realize q a
realize (TExp x) = do
  env <- envR
  unpack' . TExp . elabwhnf env <$> Q.realize x

queryR :: (Query q) => Realize q a -> q (Answer a)
queryR = Q.query

query :: (Query q, SeriableE a) => TExp a -> q (Answer a)
query = queryR . Seri.Enoch.SMT.realize

run :: (Query q) => TExp (QueryT a) -> q (TExp a)
run (TExp x) = TExp <$> Q.run x

run' :: (Query q, SeriableE a) => TExp (QueryT a) -> q a
run' x = do
  env <- envQ
  TExp v <- run x
  return $ unpack' (TExp (elabwhnf env v))

