
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

