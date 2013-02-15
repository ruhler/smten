
{-# LANGUAGE PatternGuards #-}

module Smten.HaskellF.Lib.Symbolic (
    Used, Symbolic, SMT,
    Maybe(Just, Nothing),
    __caseJust, __caseNothing,

    __prim_free_Bool, __prim_free_Integer, __prim_free_Bit,
    assert, query_Used, nest, use, used,
    return_symbolic, bind_symbolic, nobind_symbolic, fail_symbolic,
    return_smt, bind_smt, nobind_smt, fail_smt,
    runSMT,
    ) where

import Prelude hiding (Bool, Integer, IO, Char, String, Maybe(..))
import qualified Prelude as P

import Smten.Name
import Smten.Type
import Smten.ExpH
import Smten.HaskellF.Symbolic hiding (Symbolic)
import qualified Smten.HaskellF.Symbolic as S
import Smten.HaskellF.Lib.Prelude
import Smten.SMT.Primitives

newtype Used a = Used ExpH

instance SmtenT1 Used where
    smtenT1 _ = conT (name "Used")

instance Symbolic1 Used where
    box1 = Used
    unbox1 (Used x) = x

newtype Symbolic a = Symbolic ExpH

instance SmtenT1 Symbolic where
    smtenT1 _ = conT (name "Symbolic")

instance Symbolic1 Symbolic where
    box1 = Symbolic
    unbox1 (Symbolic x) = x

newtype SMT a = SMT ExpH

instance SmtenT1 SMT where
    smtenT1 _ = conT (name "SMT")

instance Symbolic1 SMT where
    box1 = SMT
    unbox1 (SMT x) = x

data Maybe a =
      Just a
    | Nothing
    | Maybe__s ExpH

instance SmtenT1 Maybe where
    smtenT1 _ = conT (name "Maybe")

instance Symbolic1 Maybe where
    box1 e
      | P.Just [a] <- de_conS "Just" e = Just (box a)
      | P.Just [] <- de_conS "Nothing" e = Nothing
      | otherwise = Maybe__s e

    unbox1 x
      | Just a <- x = conS x "Just" [unbox a]
      | Nothing <- x = conS x "Nothing" []
      | Maybe__s v <- x = v

__caseNothing :: (S.Symbolic a, S.Symbolic z) => Maybe a -> z -> z -> z
__caseNothing x y n
  | Nothing <- x = y
  | Maybe__s _ <- x = caseS "Nothing" x y n
  | otherwise = n

__caseJust :: (S.Symbolic a, S.Symbolic z) => Maybe a -> (a -> z) -> z -> z
__caseJust x y n
  | Just a <- x = y a
  | Maybe__s _ <- x = caseS "Just" x y n
  | otherwise = n

__prim_free_Bool :: Symbolic Bool
__prim_free_Bool = primS free_BoolP

__prim_free_Integer :: Symbolic Integer
__prim_free_Integer = primS free_IntegerP

__prim_free_Bit :: (S.Symbolic n) => Symbolic (Bit n)
__prim_free_Bit = primS free_BitP

assert :: Bool -> Symbolic Unit__
assert = primS assertP

use :: (S.Symbolic a) => Symbolic a -> SMT (Used a)
use = primS useP

used :: (S.Symbolic a) => Used a -> Symbolic a
used = primS usedP

query_Used :: (S.Symbolic a) => Used a -> SMT (Maybe a)
query_Used = primS query_UsedP

nest :: (S.Symbolic a) => SMT a -> SMT a
nest = primS nestP

return_smt :: (S.Symbolic a) => a -> SMT a
return_smt = primS return_SMTP

bind_smt :: (S.Symbolic a, S.Symbolic b) => SMT a -> (a -> SMT b) -> SMT b
bind_smt = primS bind_SMTP

nobind_smt :: (S.Symbolic a, S.Symbolic b) => SMT a -> SMT b -> SMT b
nobind_smt = primS nobind_SMTP

fail_smt :: (S.Symbolic a) => List__ Char -> SMT a
fail_smt = primS fail_SMTP

return_symbolic :: (S.Symbolic a) => a -> Symbolic a
return_symbolic = primS return_SymbolicP

bind_symbolic :: (S.Symbolic a, S.Symbolic b) => Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic = primS bind_SymbolicP

nobind_symbolic :: (S.Symbolic a, S.Symbolic b) => Symbolic a -> Symbolic b -> Symbolic b
nobind_symbolic = primS nobind_SymbolicP

fail_symbolic :: (S.Symbolic a) => List__ Char -> Symbolic a
fail_symbolic = primS fail_SymbolicP

runSMT :: (S.Symbolic a, S.Symbolic s) => s -> Maybe (List__ Char) -> SMT a -> IO a
runSMT = primS runSMTP

