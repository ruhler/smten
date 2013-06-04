
module Smten.SMT.Assert (Smten.SMT.Assert.assert) where

import Smten.SMT.AST as AST
import qualified Smten.Runtime.Prelude as R
import Smten.SMT.FreeID

assert :: (AST ctx exp) => ctx -> R.Bool -> IO ()
assert ctx p = {-# SCC "Assert" #-} do
    e <- bool_term ctx p
    AST.assert ctx e

bool_term :: (AST ctx exp) => ctx -> R.Bool -> IO exp
bool_term ctx R.True = bool ctx True
bool_term ctx R.False = bool ctx False
bool_term ctx (R.BoolVar id) = var ctx (freenm id)
bool_term ctx (R.BoolMux p a b) = do
    p' <- bool_term ctx p
    a' <- bool_term ctx a
    b' <- bool_term ctx b
    ite ctx p' a' b'
bool_term ctx (R.Bool__EqInteger a b) = do
    a' <- int_term ctx a
    b' <- int_term ctx b
    eq_integer ctx a' b'

int_term :: (AST ctx exp) => ctx -> R.Integer -> IO exp
int_term ctx (R.Integer i) = integer ctx i
int_term ctx (R.Integer_Add a b) = do
    a' <- int_term ctx a
    b' <- int_term ctx b
    add_integer ctx a' b'
int_term ctx (R.Integer_Sub a b) = do
    a' <- int_term ctx a
    b' <- int_term ctx b
    sub_integer ctx a' b'
int_term ctx (R.IntegerMux__ p a b) = do
    p' <- bool_term ctx p
    a' <- int_term ctx a
    b' <- int_term ctx b
    ite ctx p' a' b'
int_term ctx (R.IntegerVar id) = var ctx (freenm id)

