
module Seri.HaskellF.Lib.SMT (
    Query,
    Answer,
    __caseSatisfiable, __mkSatisfiable,
    __caseUnknown, __mkUnknown,
    __caseUnsatisfiable, __mkUnsatisfiable,

    __prim_free, assert, query, queryS,
    return_query, bind_query, nobind_query, fail_query,
    runYices1, runYices2, runSTP,
    ) where

import Prelude hiding (Bool, IO, Char, String)
import Seri.Name
import Seri.Type
import Seri.ExpH
import Seri.HaskellF.Symbolic
import Seri.HaskellF.Lib.Prelude
import Seri.SMT.Primitives

newtype Query a = Query ExpH

instance SeriT1 Query where
    seriT1 _ = conT (name "Query")

instance Symbolic1 Query where
    box1 = Query
    unbox1 (Query x) = x

newtype Answer a = Answer ExpH

instance SeriT1 Answer where
    seriT1 _ = conT (name "Answer")

instance Symbolic1 Answer where
    box1 = Answer
    unbox1 (Answer x) = x


__mkSatisfiable :: (Symbolic a) => a -> Answer a
__mkSatisfiable = conS "Satisfiable"

__mkUnsatisfiable :: (Symbolic a) => Answer a
__mkUnsatisfiable = conS "Unsatisfiable"

__mkUnknown :: (Symbolic a) => Answer a
__mkUnknown = conS "Unknown"

__caseUnknown :: (Symbolic a, Symbolic z) => Answer a -> z -> z -> z
__caseUnknown = caseS "Unknown"

__caseUnsatisfiable :: (Symbolic a, Symbolic z) => Answer a -> z -> z -> z
__caseUnsatisfiable = caseS "Unsatisfiable"

__caseSatisfiable :: (Symbolic a, Symbolic z) => Answer a -> (a -> z) -> z -> z
__caseSatisfiable = caseS "Satisfiable"

__prim_free :: (Symbolic a) => Query a
__prim_free = 
  let z = nullaryS $ __prim_freeEH (seriT z)
  in z

assert :: Bool -> Query Unit__
assert = unaryS __prim_assertEH

query :: (Symbolic a) => a -> Query (Answer a)
query = unaryS __prim_queryEH

queryS :: (Symbolic a) => Query a -> Query a
queryS = unaryS __prim_querySEH

return_query :: (Symbolic a) => a -> Query a
return_query = primS return_QueryP

bind_query :: (Symbolic a, Symbolic b) => Query a -> (a -> Query b) -> Query b
bind_query = binaryS __prim_bind_QueryEH

nobind_query :: (Symbolic a, Symbolic b) => Query a -> Query b -> Query b
nobind_query = binaryS __prim_nobind_QueryEH

fail_query :: (Symbolic a) => List__ Char -> Query a
fail_query = unaryS __prim_fail_QueryEH

runYices1 :: (Symbolic a, Symbolic d) => d -> Query a -> IO a
runYices1 = binaryS __prim_runYices1EH

runYices2 :: (Symbolic a, Symbolic d) => d -> Query a -> IO a
runYices2 = binaryS __prim_runYices2EH

runSTP :: (Symbolic a, Symbolic d) => d -> Query a -> IO a
runSTP = binaryS __prim_runSTPEH

