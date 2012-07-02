
module Yices2.Yices2 (
    Yices2.Yices2.init, exit, mkctx, run
    ) where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Foreign.Concurrent as F

import Yices2.FFI
import Yices2.Syntax

data Context = Context (ForeignPtr YContext)

-- | Initialize yices.
init :: IO ()
init = c_yices_init

-- | Exit yices
exit :: IO ()
exit = c_yices_exit


-- | Create a new yices2 context for assertions and such.
mkctx :: IO Context
mkctx = do
    ptr <- c_yices_new_context nullPtr
    fp <- F.newForeignPtr ptr (c_yices_free_context ptr)
    return $! Context fp

-- | Run a single yices command, ignoring the result.
run :: Context -> Command -> IO ()
run _ (DefineType s Nothing) = do
    ty <- c_yices_new_uninterpreted_type
    withCString s $ \str -> c_yices_set_type_name ty str
run _ (Define s ty Nothing) = do
    ty' <- ytype ty
    term <- c_yices_new_uninterpreted_term ty'
    withCString s $ \str -> c_yices_set_term_name term str
run (Context fp) (Assert p) = do
    p' <- yterm p
    withForeignPtr fp $ \yctx -> c_yices_assert_formula yctx p'
run ctx Check = check ctx >> return ()
run _ ShowModel = return ()
run _ cmd = error $ "TODO: run " ++ pretty cmd

check :: Context -> IO SMTStatus
check (Context fp) = do
    st <- withForeignPtr fp $ \yctx -> c_yices_check_context yctx nullPtr
    return (fromYSMTStatus st)

ytype :: Type -> IO YType
ytype BoolT = c_yices_bool_type
ytype IntegerT = c_yices_int_type
ytype t = error $ "TODO: ytype " ++ pretty t

binterm :: (YTerm -> YTerm -> IO YTerm) -> Expression -> Expression -> IO YTerm
binterm f a b = do
    a' <- yterm a
    b' <- yterm b
    f a' b'

yterm :: Expression -> IO YTerm
yterm (ImmediateE (VarV nm))
    = withCString nm $ \cstr -> c_yices_get_term_by_name cstr
yterm (FunctionE (ImmediateE (VarV "=")) [a, b]) = binterm c_yices_eq a b
yterm (FunctionE (ImmediateE (VarV "or")) [a, b]) = binterm c_yices_or2 a b
yterm (FunctionE (ImmediateE (VarV "xor")) [a, b]) = binterm c_yices_xor2 a b
yterm (FunctionE (ImmediateE (VarV "and")) [a, b]) = binterm c_yices_and2 a b
yterm e = error $ "TODO: yterm " ++ pretty e


