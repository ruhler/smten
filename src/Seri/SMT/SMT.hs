
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- Seri.SMT
-- 
-- Extensions to Seri which allow you to express and perform SMT queries.
module Seri.SMT.SMT where

import Seri
import Seri.Lib.Prelude

data Query a = Query
decltype ''Query

instance Monad Query where
    return = error $ "Query return"
    (>>=) = error $ "Query >>="


data Answer a = Satisfiable a | Unsatisfiable | Unknown
    deriving (Show, Eq)
decltype ''Answer

data Free a = Free Integer
decltype ''Free

declprim "free" [t| (SeriType a) => Typed Exp (Query a) |]
declprim "realize" [t| (SeriType a) => Typed Exp (Free a -> a) |]
declprim "assert" [t| Typed Exp (Bool -> Query ()) |]
declprim "query" [t| (SeriType a) => Typed Exp (a -> Query (Answer a)) |]
declprim "return" [t| (SeriType a, SeriType1 m, Monad m) => Typed Exp (a -> m a) |]
declprim ">>" [t| (SeriType a, SeriType b, SeriType1 m, Monad m) => Typed Exp (m a -> m b -> m b) |]
declprim ">>=" [t| (SeriType a, SeriType b, SeriType1 m, Monad m) => Typed Exp (m a -> (a -> m b) -> m b) |]

runQuery :: Rule -> [Dec] -> Typed Exp (Query a) -> IO (Typed Exp a)
runQuery = error $ "TODO: runQuery"

