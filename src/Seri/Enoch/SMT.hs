
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Seri.Enoch.SMT (
    Query, Answer(..), runQuery, RunOptions(..),
    free, assert, Seri.Enoch.SMT.realize, run, run',
    queryR, query,
 ) where

import Data.Functor

import Seri.Lambda hiding (free, query)
import Seri.SMT.Query hiding (free, assert, query)
import qualified Seri.SMT.Query as Q
import qualified Seri.SMT.Run as Q
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

                
instance SeriableT1 Query where
    serit1 _ = ConT (name "Query")

free :: (SeriableT a) => Query (TExp a)
free = 
    let freeE :: (SeriableT a) => TExp (Query a)
        freeE = varE "Seri.SMT.SMT.free"
    in run freeE

assert :: TExp Bool -> Query ()
assert p =
  let assertE :: TExp (Bool -> Query ())
      assertE = varE "Seri.SMT.SMT.assert"
  in run' (apply assertE p)

realize :: (SeriableE a) => TExp a -> Realize a
realize (TExp x) = unpack' . TExp <$> Q.realize x

queryR :: Realize a -> Query (Answer a)
queryR = Q.query

query :: (SeriableE a) => TExp a -> Query (Answer a)
query = queryR . Seri.Enoch.SMT.realize

run :: TExp (Query a) -> Query (TExp a)
run (TExp x) = TExp <$> Q.run x

run' :: (SeriableE a) => TExp (Query a) -> Query a
run' x = unpack' <$> run x

