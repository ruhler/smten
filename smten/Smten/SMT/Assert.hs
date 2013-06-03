
module Smten.SMT.Assert (Smten.SMT.Assert.assert) where

import Smten.SMT.AST as AST
import qualified Smten.Runtime.Prelude as R

assert :: (AST ctx exp) => ctx -> R.Bool -> IO ()
assert ctx p = {-# SCC "Assert" #-} do
    e <- bool_term ctx p
    AST.assert ctx e

bool_term :: (AST ctx exp) => ctx -> R.Bool -> IO exp
bool_term ctx R.True = bool ctx True
bool_term ctx R.False = bool ctx False

