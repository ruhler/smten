-------------------------------------------------------------------------------
-- Copyright (c) 2012      SRI International, Inc. 
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory under DARPA/AFRL contract (FA8750-10-C-0237)
-- ("CTSRD"), as part of the DARPA CRASH research programme.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
-------------------------------------------------------------------------------
--
-- Authors: 
--   Richard Uhler <ruhler@csail.mit.edu>
-- 
-------------------------------------------------------------------------------

-- | An interface to yices2.
module Yices2.Yices2 (
    Context(),
    Yices2.Yices2.init, exit, mkctx, run,
    SMTStatus(..), check,
    getIntegerValue,
    ) where

import Data.Ratio

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Yices2.FFI
import Yices2.Syntax


data Context = Context (Ptr YContext)

-- | Initialize yices.
init :: IO ()
init = c_yices_init

-- | Exit yices
exit :: IO ()
exit = c_yices_exit


-- | Create a new yices2 context for assertions and such.
-- TODO: this currently leaks context pointers!
-- That should most certainly be fixed somehow.
mkctx :: IO Context
mkctx = do
    ptr <- c_yices_new_context nullPtr
    return $! Context ptr

-- | Run a single yices command, ignoring the result.
run :: Context -> Command -> IO ()
run _ (DefineType s Nothing) = do
    ty <- c_yices_new_uninterpreted_type
    withCString s $ \str -> c_yices_set_type_name ty str
run _ (DefineType s (Just (NormalTD ty))) = do
    ty' <- ytype ty
    withCString s $ \str -> c_yices_set_type_name ty' str
run _ (DefineType s (Just (ScalarTD nms))) = do
    scalar <- c_yices_new_scalar_type (fromIntegral $ length nms)
    withCString s $ \str -> c_yices_set_type_name scalar str
    let defnm :: (String, Int32) -> IO ()
        defnm (nm, idx) = do
            v <- c_yices_constant scalar idx
            withCString nm $ \str -> c_yices_set_term_name v str
    mapM_ defnm (zip nms [0..])
run _ (Define s ty Nothing) = do
    ty' <- ytype ty
    term <- c_yices_new_uninterpreted_term ty'
    withCString s $ \str -> c_yices_set_term_name term str
run (Context yctx) (Assert p) = do
    p' <- yterm p
    c_yices_assert_formula yctx p'
run ctx Check = check ctx >> return ()
run _ ShowModel = do
    return ()
run (Context yctx) Push = do
    c_yices_push yctx
run (Context yctx) Pop = do
    c_yices_pop yctx
run _ cmd = error $ "TODO: run " ++ pretty cmd

-- | Run (check) in the given context and return the resulting yices2 status.
check :: Context -> IO SMTStatus
check (Context yctx) = do
    st <- c_yices_check_context yctx nullPtr
    return $! fromYSMTStatus st

withstderr :: (Ptr CFile -> IO a) -> IO a
withstderr f = do
    cf <- withCString "w" $ \str -> c_fdopen 2 str
    x <- f cf 
    return $! x

ytype :: Type -> IO YType
ytype t = do
    yt <- withCString (pretty t) $ \str -> c_yices_parse_type str
    if yt < 0
        then do
            withstderr $ \stderr -> c_yices_print_error stderr
            error $ "ytype: " ++ pretty t
        else do
            return $! yt

isbinop :: String -> Expression -> Bool
isbinop nm (FunctionE (ImmediateE (VarV n)) [_, _]) = n == nm
isbinop _ _ = False

dobinop :: [(String, YTerm)] -> Expression -> (YTerm -> YTerm -> IO YTerm) -> IO YTerm
dobinop s (FunctionE (ImmediateE (VarV _)) [a, b]) f = do
    at <- ytermS s a
    bt <- ytermS s b
    f at bt

yterm :: Expression -> IO YTerm
yterm = ytermS []

builtin :: [String]
builtin = [
    "true", "false", "if", "ite", "=", "/=", "distinct",
    "or", "and", "not", "xor", "<=>", "=>", "mk-tuple",
    "select", "tuple-update", "update",  "forall",  "exists",  "let",
    "+", "-", "*", "/", "^",
    "<", "<=", ">", ">=",
    "mk-bv", "bv-add", "bv-sub", "bv-mul", "bv-neg", "bv-pow",
    "bv-not", "bv-and", "bv-or", "bv-xor", "bv-nand", "bv-nor", "bv-xnor",
    "bv-shift-left0", "bv-shift-left1", "bv-shift-right0", "bv-shift-right1",
    "bv-ashift-right", "bv-rotate-left", "bv-rotate-right",
    "bv-extract", "bv-concat", "bv-repeat",
    "bv-sign-extend", "bv-zero-extend", "bv-ge", "bv-gt", "bv-le", "bv-lt",
    "bv-sge", "bv-sgt", "bv-sle", "bv-slt", "bv-shl", "bv-lshr", "bv-ashr",
    "bv-div", "bv-rem", "bv-sdiv", "bv-srem", "bv-smod",
    "bv-redor", "bv-redand", "bv-comp"
    ]


-- Construct a yices term from the given expression with the given
-- variable scope.
ytermS :: [(String, YTerm)] -> Expression -> IO YTerm
ytermS s e | isbinop "=" e = dobinop s e c_yices_eq
ytermS s e | isbinop "<" e = dobinop s e c_yices_arith_lt_atom
ytermS s e | isbinop ">" e = dobinop s e c_yices_arith_gt_atom
ytermS s e | isbinop "+" e = dobinop s e c_yices_add
ytermS s e | isbinop "-" e = dobinop s e c_yices_sub
ytermS s e | isbinop "*" e = dobinop s e c_yices_mul
ytermS s e | isbinop "or" e = dobinop s e c_yices_or2
ytermS s e | isbinop "and" e = dobinop s e c_yices_and2
ytermS s e | isbinop "xor" e = dobinop s e c_yices_xor2
ytermS s (FunctionE (ImmediateE (VarV "tuple-update")) [a, ImmediateE (RationalV i), v]) = do
    at <- ytermS s a
    vt <- ytermS s v
    c_yices_tuple_update at (fromInteger $ numerator i) vt
ytermS s (FunctionE (ImmediateE (VarV "select")) [v, ImmediateE (RationalV i)]) = do
    vt <- ytermS s v
    c_yices_select (fromInteger $ numerator i) vt
ytermS s (FunctionE (ImmediateE (VarV "if")) [p, a, b]) = do
    pt <- ytermS s p 
    at <- ytermS s a
    bt <- ytermS s b
    c_yices_ite pt at bt
ytermS s (FunctionE (ImmediateE (VarV "mk-tuple")) args) = do
    argst <- mapM (ytermS s) args
    withArray argst $ c_yices_tuple (fromIntegral $ length argst)
ytermS s (FunctionE (ImmediateE (VarV "and")) args) = do
    argst <- mapM (ytermS s) args
    withArray argst $ c_yices_and (fromIntegral $ length argst)
ytermS s e@(FunctionE (ImmediateE (VarV f)) _) | f `elem` builtin = do
    error $ "TODO: yterm builtin " ++ pretty e
ytermS s (FunctionE f [a]) = do
    ft <- ytermS s f
    at <- ytermS s a 
    withArray [at] $ c_yices_application ft 1
ytermS s (UpdateE f args v) = do
    ft <- ytermS s f
    argst <- mapM (ytermS s) args
    vt <- ytermS s v
    withArray argst $ \arr -> c_yices_update ft (fromIntegral $ length argst) arr vt
ytermS s (LetE bs e) = 
  let mkvar :: (String, Expression) -> IO (String, YTerm)
      mkvar (nm, e) = do
        et <- ytermS s e
        return (nm, et)
  in do
    vars <- mapM mkvar bs
    ytermS (vars ++ s) e
        
ytermS _ (ImmediateE TrueV) = c_yices_true
ytermS _ (ImmediateE FalseV) = c_yices_false
ytermS _ (ImmediateE (RationalV r)) = do
    c_yices_rational64 (fromInteger $ numerator r) (fromInteger $ denominator r)
ytermS s e@(ImmediateE (VarV nm)) =
    case lookup nm s of
        Nothing -> ytermbystr e
        Just t -> return t
    
ytermS _ e = error $ "TODO: yterm: " ++ pretty e


-- | Construct a yices term for the given expression. This works by printing
-- the expression to a string and passing the string over to yices to parse.
ytermbystr :: Expression -> IO YTerm
ytermbystr e = do
    ye <- withCString (pretty e) $ \str -> c_yices_parse_term str
    if ye < 0 
        then do 
            withstderr $ \stderr -> c_yices_print_error stderr
            error $ "yterm error"
        else return $! ye

-- | Given the name of a free variable with integer type, return its value.
getIntegerValue :: Context -> String -> IO Integer
getIntegerValue (Context yctx) nm = do
    model <- c_yices_get_model yctx 1
    x <- alloca $ \ptr -> do
            term <- yterm (varE nm)
            ir <- c_yices_get_int64_value model term ptr
            if ir == 0
               then do 
                  v <- peek ptr
                  return $! v
               else error $ "yices2 get int64 value returned: " ++ show ir
    c_yices_free_model model
    return $! toInteger x

