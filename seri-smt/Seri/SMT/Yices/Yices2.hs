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

{-# LANGUAGE PatternGuards #-}

-- | Backend for the Yices2 solver
module Seri.SMT.Yices.Yices2 (Yices2(), yices2) where

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

data Yices2 = Yices2 (Ptr YContext)

instance Solver Yices2 where
    pretty _ = YC.pretty

    -- TODO: this currently leaks context pointers!
    -- That should most certainly be fixed somehow.
    -- TODO: when do we call c_yices_exit?
    initialize = do
        c_yices_init
        ptr <- c_yices_new_context nullPtr
        return $! Yices2 ptr

    run _ (Declare s ty) = do
        ty' <- ytype ty
        term <- c_yices_new_uninterpreted_term ty'
        withCString s $ c_yices_set_term_name term
    run (Yices2 yctx) (Assert p) = do
        p' <- yterm p
        c_yices_assert_formula yctx p'
    run ctx Check = check ctx >> return ()
    run (Yices2 yctx) Push = do
        c_yices_push yctx
    run (Yices2 yctx) Pop = do
        c_yices_pop yctx

    check (Yices2 yctx) = do
        st <- c_yices_check_context yctx nullPtr
        return $! fromYSMTStatus st

    getIntegerValue (Yices2 yctx) nm = do
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

    getBoolValue (Yices2 yctx) nm = do
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
        
    getBitVectorValue (Yices2 yctx) w nm = do
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
isbinop nm (AppE (VarE n) [_, _]) = n == nm
isbinop _ _ = False

dobinop' :: [(String, YTerm)] -> Expression -> Expression -> (YTerm -> YTerm -> IO YTerm) -> IO YTerm
dobinop' s a b f = do
    at <- ytermS s a
    bt <- ytermS s b
    f at bt

dobinop :: [(String, YTerm)] -> Expression -> (YTerm -> YTerm -> IO YTerm) -> IO YTerm
dobinop s (AppE (VarE _) [a, b]) f = dobinop' s a b f

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
ytermS s e | Just (a, b) <- de_eqE e = dobinop' s a b c_yices_eq
ytermS s e | Just [a, b] <- de_orE e = dobinop' s a b c_yices_or2
ytermS s e | Just [a, b] <- de_andE e = dobinop' s a b c_yices_and2
ytermS s e | Just (a, b) <- de_bvaddE e = dobinop' s a b c_yices_bvadd
ytermS s e | Just (a, b) <- de_bvsubE e = dobinop' s a b c_yices_bvsub
ytermS s e | Just (a, b) <- de_bvorE e = dobinop' s a b c_yices_bvor
ytermS s e | Just (a, b) <- de_bvandE e = dobinop' s a b c_yices_bvand
ytermS s e | Just (a, b) <- de_bvconcatE e = dobinop' s a b c_yices_bvconcat
ytermS s e | isbinop "<" e = dobinop s e c_yices_arith_lt_atom
ytermS s e | isbinop "<=" e = dobinop s e c_yices_arith_leq_atom
ytermS s e | isbinop ">" e = dobinop s e c_yices_arith_gt_atom
ytermS s e | isbinop "+" e = dobinop s e c_yices_add
ytermS s e | isbinop "-" e = dobinop s e c_yices_sub
ytermS s e | isbinop "*" e = dobinop s e c_yices_mul
ytermS s e | isbinop "xor" e = dobinop s e c_yices_xor2
ytermS s e | isbinop "bv-sub" e = dobinop s e c_yices_bvsub
ytermS s e | isbinop "bv-mul" e = dobinop s e c_yices_bvmul
ytermS s e | isbinop "bv-shl" e = dobinop s e c_yices_bvshl
ytermS s e | Just a <- de_notE e = do
    at <- ytermS s a
    c_yices_not at
ytermS s e | Just a <- de_bvnotE e = do
    at <- ytermS s a
    c_yices_bvnot at
ytermS s (AppE (VarE "select") [v, LitE (IntegerL i)]) = do
    vt <- ytermS s v
    c_yices_select (fromInteger i) vt
ytermS s e | Just (p, a, b) <- de_ifE e = do
    pt <- ytermS s p 
    at <- ytermS s a
    bt <- ytermS s b
    c_yices_ite pt at bt
ytermS s (AppE (VarE "mk-tuple") args) = do
    argst <- mapM (ytermS s) args
    withArray argst $ c_yices_tuple (fromIntegral $ length argst)
ytermS s e | Just (w, v) <- de_mkbvE e = do
    c_yices_bvconst_uint64 (fromInteger w) (fromInteger v)
ytermS s e | Just (a, n) <- de_bvzeroExtendE e = do
    at <- ytermS s a
    c_yices_zero_extend at (fromInteger n)

-- syntax for bv-extract is: bv-extract end begin bv
-- for the c api, we have: bvextract bv begin end
ytermS s (AppE (VarE "bv-extract") [LitE (IntegerL end), LitE (IntegerL begin), x]) = do
    xt <- ytermS s x
    c_yices_bvextract xt (fromInteger begin) (fromInteger end)

ytermS s e | Just (a, n) <- de_bvshiftLeft0E e = do
    at <- ytermS s a
    c_yices_shift_left0 at (fromInteger n)
ytermS s (AppE (VarE "bv-shift-right0") [a, LitE (IntegerL n)]) = do
    at <- ytermS s a
    c_yices_shift_right0 at (fromInteger n)
ytermS s (AppE (VarE "and") args) = do
    argst <- mapM (ytermS s) args
    withArray argst $ c_yices_and (fromIntegral $ length argst)
ytermS s (AppE (VarE "or") args) = do
    argst <- mapM (ytermS s) args
    withArray argst $ c_yices_or (fromIntegral $ length argst)
ytermS s e@(AppE (VarE f) _) | f `elem` builtin = do
    error $ "TODO: yterm builtin " ++ YC.pretty e
ytermS s (AppE f [a]) = do
    ft <- ytermS s f
    at <- ytermS s a 
    withArray [at] $ c_yices_application ft 1
ytermS s (UpdateE f args v) = do
    ft <- ytermS s f
    argst <- mapM (ytermS s) args
    vt <- ytermS s v
    withArray argst $ \arr -> c_yices_update ft (fromIntegral $ length argst) arr vt
ytermS s e | Just (bs, v) <- de_letE e =
  let mkvar :: (String, Expression) -> IO (String, YTerm)
      mkvar (nm, e) = do
        et <- ytermS s e
        return (nm, et)
  in do
    vars <- mapM mkvar bs
    ytermS (vars ++ s) v
        
ytermS _ (LitE (BoolL True)) = c_yices_true
ytermS _ (LitE (BoolL False)) = c_yices_false
ytermS _ (LitE (IntegerL i)) = c_yices_int64 (fromInteger i)
ytermS s e@(VarE nm) =
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

yices2 :: Q.Query Yices2 a -> Q.Query Yices2 a
yices2 = id

