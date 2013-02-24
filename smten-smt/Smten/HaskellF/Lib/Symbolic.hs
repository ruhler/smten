
{-# LANGUAGE PatternGuards #-}

module Smten.HaskellF.Lib.Symbolic (
    Used, Symbolic, SMT,
    Maybe(Just, Nothing),
    __caseJust, __caseNothing,

    __prim_free_Bool, __prim_free_Integer, __prim_free_Bit,
    assert, query_Used, nest, use, used,
    return_symbolic, bind_symbolic, nobind_symbolic, fail_symbolic,
    return_smt, bind_smt, nobind_smt, fail_smt,
    runSMT, liftIO_SMT,
    ) where

import Prelude hiding (Bool, Integer, IO, Char, String, Maybe(..))
import qualified Prelude as P

import Smten.Name
import Smten.Type
import Smten.ExpH
import Smten.HaskellF.HaskellF
import Smten.HaskellF.Lib.Prelude
import Smten.SMT.Primitives

newtype Used a = Used ExpH

instance SmtenT1 Used where
    smtenT1 _ = conT (name "Used")

instance HaskellF1 Used where
    box1 = Used
    unbox1 (Used x) = x

newtype Symbolic a = Symbolic ExpH

instance SmtenT1 Symbolic where
    smtenT1 _ = conT (name "Symbolic")

instance HaskellF1 Symbolic where
    box1 = Symbolic
    unbox1 (Symbolic x) = x

newtype SMT a = SMT ExpH

instance SmtenT1 SMT where
    smtenT1 _ = conT (name "SMT")

instance HaskellF1 SMT where
    box1 = SMT
    unbox1 (SMT x) = x

data Maybe a =
      Just a
    | Nothing
    | Maybe__s ExpH

instance SmtenT1 Maybe where
    smtenT1 _ = conT (name "Maybe")

instance HaskellF1 Maybe where
    box1 e
      | P.Just [a] <- de_conHF "Just" e = Just (box a)
      | P.Just [] <- de_conHF "Nothing" e = Nothing
      | otherwise = Maybe__s e

    unbox1 x
      | Just a <- x = conHF x "Just" [unbox a]
      | Nothing <- x = conHF x "Nothing" []
      | Maybe__s v <- x = v

__caseNothing :: (HaskellF a, HaskellF z) => Maybe a -> z -> z -> z
__caseNothing x y n
  | Nothing <- x = y
  | Maybe__s _ <- x = caseHF "Nothing" x y n
  | otherwise = n

__caseJust :: (HaskellF a, HaskellF z) => Maybe a -> (a -> z) -> z -> z
__caseJust x y n
  | Just a <- x = y a
  | Maybe__s _ <- x = caseHF "Just" x y n
  | otherwise = n

__prim_free_Bool :: Symbolic Bool
__prim_free_Bool = primHF free_BoolP

__prim_free_Integer :: Symbolic Integer
__prim_free_Integer = primHF free_IntegerP

__prim_free_Bit :: (HaskellF n) => Symbolic (Bit n)
__prim_free_Bit = primHF free_BitP

assert :: Bool -> Symbolic Unit__
assert = primHF assertP

use :: (HaskellF a) => Symbolic a -> SMT (Used a)
use = primHF useP

used :: (HaskellF a) => Used a -> Symbolic a
used = primHF usedP

query_Used :: (HaskellF a) => Used a -> SMT (Maybe a)
query_Used = primHF query_UsedP

nest :: (HaskellF a) => SMT a -> SMT a
nest = primHF nestP

return_smt :: (HaskellF a) => a -> SMT a
return_smt = primHF return_SMTP

bind_smt :: (HaskellF a, HaskellF b) => SMT a -> (a -> SMT b) -> SMT b
bind_smt = primHF bind_SMTP

nobind_smt :: (HaskellF a, HaskellF b) => SMT a -> SMT b -> SMT b
nobind_smt = primHF nobind_SMTP

fail_smt :: (HaskellF a) => List__ Char -> SMT a
fail_smt = primHF fail_SMTP

return_symbolic :: (HaskellF a) => a -> Symbolic a
return_symbolic = primHF return_SymbolicP

bind_symbolic :: (HaskellF a, HaskellF b) => Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic = primHF bind_SymbolicP

nobind_symbolic :: (HaskellF a, HaskellF b) => Symbolic a -> Symbolic b -> Symbolic b
nobind_symbolic = primHF nobind_SymbolicP

fail_symbolic :: (HaskellF a) => List__ Char -> Symbolic a
fail_symbolic = primHF fail_SymbolicP

runSMT :: (HaskellF a, HaskellF s) => s -> Maybe (List__ Char) -> SMT a -> IO a
runSMT = primHF runSMTP

liftIO_SMT :: (HaskellF a) => IO a -> SMT a
liftIO_SMT = primHF liftIO_SMTP

