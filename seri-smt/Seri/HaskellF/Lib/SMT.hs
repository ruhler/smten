
{-# LANGUAGE PatternGuards #-}

module Seri.HaskellF.Lib.SMT (
    Query,
    Answer(Satisfiable, Unsatisfiable, Unknown),
    __caseSatisfiable, __caseUnknown, __caseUnsatisfiable,

    __prim_free_Bool, __prim_free_Integer, __prim_free_Bit,
    assert, query, queryS,
    return_query, bind_query, nobind_query, fail_query,
    runYices1, runYices2, runSTP,
    ) where

import Prelude hiding (Bool, Integer, IO, Char, String)
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

data Answer a =
      Satisfiable a
    | Unsatisfiable
    | Unknown
    | Answer__s ExpH

instance SeriT1 Answer where
    seriT1 _ = conT (name "Answer")

instance Symbolic1 Answer where
    box1 e
      | Just [a] <- de_conS "Satisfiable" e = Satisfiable (box a)
      | Just [] <- de_conS "Unsatisfiable" e = Unsatisfiable
      | Just [] <- de_conS "Unknown" e = Unknown
      | otherwise = Answer__s e

    unbox1 x
      | Satisfiable a <- x = conS x "Satisfiable" [unbox a]
      | Unsatisfiable <- x = conS x "Unsatisfiable" []
      | Unknown <- x = conS x "Unknown" []
      | Answer__s v <- x = v

__caseUnknown :: (Symbolic a, Symbolic z) => Answer a -> z -> z -> z
__caseUnknown x y n
  | Unknown <- x = y
  | Answer__s _ <- x = caseS "Unknown" x y n
  | otherwise = n

__caseUnsatisfiable :: (Symbolic a, Symbolic z) => Answer a -> z -> z -> z
__caseUnsatisfiable x y n
  | Unsatisfiable <- x = y
  | Answer__s _ <- x = caseS "Unsatisfiable" x y n
  | otherwise = n

__caseSatisfiable :: (Symbolic a, Symbolic z) => Answer a -> (a -> z) -> z -> z
__caseSatisfiable x y n
  | Satisfiable a <- x = y a
  | Answer__s _ <- x = caseS "Satisfiable" x y n
  | otherwise = n

__prim_free_Bool :: Query Bool
__prim_free_Bool = primS free_BoolP

__prim_free_Integer :: Query Integer
__prim_free_Integer = primS free_IntegerP

__prim_free_Bit :: (Symbolic n) => Query (Bit n)
__prim_free_Bit = primS free_BitP

assert :: Bool -> Query Unit__
assert = primS assertP

query :: (Symbolic a) => a -> Query (Answer a)
query = primS queryP

queryS :: (Symbolic a) => Query a -> Query a
queryS = primS querySP

return_query :: (Symbolic a) => a -> Query a
return_query = primS return_QueryP

bind_query :: (Symbolic a, Symbolic b) => Query a -> (a -> Query b) -> Query b
bind_query = primS bind_QueryP

nobind_query :: (Symbolic a, Symbolic b) => Query a -> Query b -> Query b
nobind_query = primS nobind_QueryP

fail_query :: (Symbolic a) => List__ Char -> Query a
fail_query = primS fail_QueryP

runYices1 :: (Symbolic a, Symbolic d) => d -> Query a -> IO a
runYices1 = primS runYices1P

runYices2 :: (Symbolic a, Symbolic d) => d -> Query a -> IO a
runYices2 = primS runYices2P

runSTP :: (Symbolic a, Symbolic d) => d -> Query a -> IO a
runSTP = primS runSTPP

