
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- Seri.SMT
-- 
-- Extensions to Seri which allow you to express and perform SMT queries.
module Seri.SMT.SMT where

import Seri
import Seri.Lib.Prelude


[s|
    data Query a = Query

    data Answer a = Satisfiable a | Unsatisfiable | Unknown
        deriving (Show, Eq)

    data Free a = Free Integer
|]

declprim "free" [t| forall a. Query a |]
declprim "assert" [t| Bool -> Query () |]
declprim "query" [t| forall a. a -> Query (Answer a) |]
declprim "scoped" [t| forall a. Query a -> Query a |]

declprim "return_query" [t| forall a . a -> Query a |]
declprim "nobind_query" [t| forall a b . Query a -> Query b -> Query b |]
declprim "bind_query" [t| forall a b . Query a -> (a -> Query b) -> Query b |]
declprim "fail_query" [t| forall a . String -> Query a |]

[s|
    instance Monad Query where
        return = return_query
        (>>=) = bind_query
        (>>) = nobind_query
        fail = fail_query
|]

runQuery :: Typed Exp (Query a) -> IO (Typed Exp a)
runQuery = error $ "TODO: runQuery"

