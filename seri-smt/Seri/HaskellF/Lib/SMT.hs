
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Seri.HaskellF.Lib.SMT (
    Query, Answer,
    __prim_free,
    Q.assert, Q.query, Q.queryS,
    return_query, bind_query, nobind_query, fail_query,
    runYices1, runYices2, runSTP,

    __caseSatisfiable, __mkSatisfiable,
    __caseUnknown, __mkUnknown,
    __caseUnsatisfiable, __mkUnsatisfiable,
    ) where

import qualified Seri.HaskellF.Lib.Prelude as F
import qualified Seri.HaskellF.Lib.Query as Q
import qualified Seri.SMT.Yices.Yices1 as Q
import qualified Seri.SMT.Yices.Yices2 as Q
import qualified Seri.SMT.STP.STP as Q

type Query = Q.Query
type Answer = Q.Answer

class Free a where
    free :: Query a

instance Free F.Bool where
    free = Q.freebool

instance Free F.Integer where
    free = error $ "TODO: free Integer"

instance Free (F.Bit n) where
    free = error $ "TODO: free Bit n"

instance Free (a -> b) where
    free = error $ "TODO: free a -> b"

__prim_free :: (Free a) => Query a
__prim_free = free

return_query :: a -> Query a
return_query = return

bind_query :: Query a -> (a -> Query b) -> Query b
bind_query = (>>=)

nobind_query :: Query a -> Query b -> Query b
nobind_query = (>>)

fail_query :: String -> Query a
fail_query = fail

-- TODO: don't ignore debug argument.
runYices1 :: d -> Query a -> IO a
runYices1 _ q = do
    s <- Q.yices1 
    Q.runQuery (Q.RunOptions (Just "foo.yices1.dbg") s) q

-- TODO: don't ignore debug argument.
runYices2 :: d -> Query a -> IO a
runYices2 _ q = do
    s <- Q.yices2 
    Q.runQuery (Q.RunOptions (Just "foo.yices2.dbg") s) q

-- TODO: don't ignore debug argument.
runSTP :: d -> Query a -> IO a
runSTP _ q = do
    s <- Q.stp
    Q.runQuery (Q.RunOptions (Just "foo.stp.dbg") s) q

__mkSatisfiable :: a -> Answer a
__mkSatisfiable = Q.Satisfiable

__mkUnsatisfiable :: Answer a
__mkUnsatisfiable = Q.Unsatisfiable

__mkUnknown :: Answer a
__mkUnknown = Q.Unknown

__caseUnknown :: Answer a -> x -> x -> x
__caseUnknown Q.Unknown y _ = y
__caseUnknown _ _ n = n

__caseUnsatisfiable :: Answer a -> x -> x -> x
__caseUnsatisfiable Q.Unsatisfiable y _ = y
__caseUnsatisfiable _ _ n = n

__caseSatisfiable :: Answer a -> (a -> x) -> x -> x
__caseSatisfiable (Q.Satisfiable a) y _ = y a
__caseSatisfiable _ _ n = n

instance F.Symbolic1__ Query where
    __default1 = return_query F.__default

instance F.Symbolic1__ Answer where
    __default1 = __mkUnsatisfiable

