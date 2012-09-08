
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
import Seri.Enoch.Prelude

instance SeriableT1 Answer where
    serit1 x = ConT (name "Answer")

instance (SeriableE a) => SeriableE (Answer a) where
    pack (Satisfiable x) =
      let satE :: (SeriableT a) => TExp (a -> Answer a)
          satE = conE "Satisfiable"
      in apply satE (pack x)
    pack Unsatisfiable =
      let unsatE :: (SeriableT a) => TExp (Answer a)
          unsatE = conE "Unsatisfiable"
      in unsatE
    pack Unknown =
      let unknownE :: (SeriableT a) => TExp (Answer a)
          unknownE = conE "Unknown"
      in unknownE

    unpack (TExp (AppE (ConE (Sig n _)) x)) | n Prelude.== name "Satisfiable" = do
        a <- unpack (TExp x)
        return $ Satisfiable a
    unpack (TExp (ConE (Sig n _))) | n Prelude.== name "Unsatisfiable" = return Unsatisfiable
    unpack (TExp (ConE (Sig n _))) | n Prelude.== name "Unknown" = return Unknown
    unpack _ = Nothing

-- Dummy query type with instance of SeriableT.
data QueryT a = QueryT

instance SeriableT1 QueryT where
    serit1 _ = ConT (name "Query")

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
realize (TExp x) = unpack' . TExp <$> Q.realize x

queryR :: (Query q) => Realize q a -> q (Answer a)
queryR = Q.query

query :: (Query q, SeriableE a) => TExp a -> q (Answer a)
query = queryR . Seri.Enoch.SMT.realize

run :: (Query q) => TExp (QueryT a) -> q (TExp a)
run (TExp x) = TExp <$> Q.run x

run' :: (Query q, SeriableE a) => TExp (QueryT a) -> q a
run' x = unpack' <$> run x

