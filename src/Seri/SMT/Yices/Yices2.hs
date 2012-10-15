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
module Seri.SMT.Yices.Yices2 (Yices2FFI(), yices2) where

import Data.List(genericLength)
import Data.Ratio

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Seri.SMT.Yices.FFI2
import Seri.SMT.Syntax
import qualified Seri.SMT.Yices.Concrete as YC
import Seri.SMT.Solver
import qualified Seri.SMT.Query as Q

data Yices2FFI = Yices2FFI (Ptr YContext)

instance Solver Yices2FFI where
    pretty _ = YC.pretty

    -- TODO: this currently leaks context pointers!
    -- That should most certainly be fixed somehow.
    -- TODO: when do we call c_yices_exit?
    initialize = do
        c_yices_init
        ptr <- c_yices_new_context nullPtr
        return $! Yices2FFI ptr

    run _ (Define s ty Nothing) = do
        ty' <- ytype ty
        term <- c_yices_new_uninterpreted_term ty'
        withCString s $ c_yices_set_term_name term
    run _ (Define s _ (Just e)) = do
        term <- yterm e
        withCString s $ c_yices_set_term_name term
    run (Yices2FFI yctx) (Assert p) = do
        p' <- yterm p
        c_yices_assert_formula yctx p'
    run ctx Check = check ctx >> return ()
    run (Yices2FFI yctx) Push = do
        c_yices_push yctx
    run (Yices2FFI yctx) Pop = do
        c_yices_pop yctx

    check (Yices2FFI yctx) = do
        st <- c_yices_check_context yctx nullPtr
        return $! fromYSMTStatus st

    getIntegerValue (Yices2FFI yctx) nm = do
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

    getBoolValue (Yices2FFI yctx) nm = do
        model <- c_yices_get_model yctx 1
        x <- alloca $ \ptr -> do
                term <- yterm (varE nm)
                ir <- c_yices_get_bool_value model term ptr
                case ir of
                   _ | ir == (-1) -> do
                      -- -1 means we don't care, so just return the equivalent
                      -- of False.
                      return 0

                   0 -> do 
                      v <- peek ptr
                      return v

                   _ -> error $ "yices2 get bool value returned: " ++ show ir
        c_yices_free_model model
        case x of
            0 -> return False
            1 -> return True
            _ -> error $ "yices2 get bool value got: " ++ show x
        
    getBitVectorValue (Yices2FFI yctx) w nm = do
        model <- c_yices_get_model yctx 1
        bits <- allocaArray (fromInteger w) $ \ptr -> do
            term <- yterm (varE nm)
            ir <- c_yices_get_bv_value model term ptr
            if ir == 0
                then peekArray (fromInteger w) ptr
                else error $ "yices2 get bit vector value returned: " ++ show ir
        c_yices_free_model model
        return $! bvInteger bits
        
                

bvInteger :: [Int32] -> Integer
bvInteger [] = 0
bvInteger (x:xs) = bvInteger xs * 2 + (fromIntegral x)

withstderr :: (Ptr CFile -> IO a) -> IO a
withstderr f = do
    cf <- withCString "w" $ \str -> c_fdopen 2 str
    x <- f cf 
    return $! x

ytype :: Type -> IO YType
ytype (ArrowT ts) = do
    tr <- ytype (last ts)
    ts' <- mapM ytype (init ts)
    withArray ts' $ \arr -> c_yices_function_type (genericLength ts') arr tr
ytype (BitVectorT i) = c_yices_bv_type (fromIntegral i)
ytype (IntegerT) = c_yices_int_type
ytype (BoolT) = c_yices_bool_type
    

ytypebystr :: Type -> IO YType
ytypebystr t = do
    yt <- withCString (YC.concrete t) $ \str -> c_yices_parse_type str
    if yt < 0
        then do
            withstderr $ \stderr -> c_yices_print_error stderr
            error $ "ytype: " ++ YC.pretty t
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
ytermS s e | isbinop "<=" e = dobinop s e c_yices_arith_leq_atom
ytermS s e | isbinop ">" e = dobinop s e c_yices_arith_gt_atom
ytermS s e | isbinop "+" e = dobinop s e c_yices_add
ytermS s e | isbinop "-" e = dobinop s e c_yices_sub
ytermS s e | isbinop "*" e = dobinop s e c_yices_mul
ytermS s e | isbinop "or" e = dobinop s e c_yices_or2
ytermS s e | isbinop "and" e = dobinop s e c_yices_and2
ytermS s e | isbinop "xor" e = dobinop s e c_yices_xor2
ytermS s e | isbinop "bv-add" e = dobinop s e c_yices_bvadd
ytermS s e | isbinop "bv-sub" e = dobinop s e c_yices_bvsub
ytermS s e | isbinop "bv-mul" e = dobinop s e c_yices_bvmul
ytermS s e | isbinop "bv-or" e = dobinop s e c_yices_bvor
ytermS s e | isbinop "bv-and" e = dobinop s e c_yices_bvand
ytermS s e | isbinop "bv-shl" e = dobinop s e c_yices_bvshl
ytermS s (FunctionE (ImmediateE (VarV "not")) [e]) = do
    et <- ytermS s e
    c_yices_not et
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
ytermS s (FunctionE (ImmediateE (VarV "mk-bv")) [ImmediateE (RationalV w), ImmediateE (RationalV v)]) = do
    c_yices_bvconst_uint64 (fromInteger $ numerator w) (fromInteger $ numerator v)
ytermS s (FunctionE (ImmediateE (VarV "bv-zero-extend")) [a, ImmediateE (RationalV n)]) = do
    at <- ytermS s a
    c_yices_zero_extend at (fromInteger $ numerator n)

-- syntax for bv-extract is: bv-extract end begin bv
-- for the c api, we have: bvextract bv begin end
ytermS s (FunctionE (ImmediateE (VarV "bv-extract")) [ImmediateE (RationalV end), ImmediateE (RationalV begin), x]) = do
    xt <- ytermS s x
    c_yices_bvextract xt (fromInteger $ numerator begin) (fromInteger $ numerator end)

ytermS s (FunctionE (ImmediateE (VarV "bv-shift-left0")) [a, ImmediateE (RationalV n)]) = do
    at <- ytermS s a
    c_yices_shift_left0 at (fromInteger $ numerator n)
ytermS s (FunctionE (ImmediateE (VarV "bv-shift-right0")) [a, ImmediateE (RationalV n)]) = do
    at <- ytermS s a
    c_yices_shift_right0 at (fromInteger $ numerator n)
ytermS s (FunctionE (ImmediateE (VarV "and")) args) = do
    argst <- mapM (ytermS s) args
    withArray argst $ c_yices_and (fromIntegral $ length argst)
ytermS s (FunctionE (ImmediateE (VarV "or")) args) = do
    argst <- mapM (ytermS s) args
    withArray argst $ c_yices_or (fromIntegral $ length argst)
ytermS s e@(FunctionE (ImmediateE (VarV f)) _) | f `elem` builtin = do
    error $ "TODO: yterm builtin " ++ YC.pretty e
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
        Nothing -> withCString nm c_yices_get_term_by_name
        Just t -> return t 
    
ytermS _ e = error $ "TODO: yterm: " ++ YC.pretty e


-- | Construct a yices term for the given expression. This works by printing
-- the expression to a string and passing the string over to yices to parse.
ytermbystr :: Expression -> IO YTerm
ytermbystr e = do
    ye <- withCString (YC.concrete e) $ \str -> c_yices_parse_term str
    if ye < 0 
        then do 
            withstderr $ \stderr -> c_yices_print_error stderr
            error $ "yterm error"
        else return $! ye

yices2 :: Q.Query Yices2FFI a -> Q.Query Yices2FFI a
yices2 = id

