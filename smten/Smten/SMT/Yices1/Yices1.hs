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

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}

-- | Backend for the Yices1 Solver
--
-- NOTE: This assumes all the symbols from the yices1 library starting with
-- yices_ have been renamed to yices1_. This is so yices1 and yices2 can
-- coexist.
module Smten.SMT.Yices1.Yices1 (yices1) where

import Foreign hiding (bit)
import Foreign.C.String
import Foreign.C.Types

import Data.Dynamic
import Data.Functor ((<$>))

import Smten.SMT.Yices1.FFI
import Smten.SMT.Solver

data Yices1 = Yices1 {
    y1_ctx :: Ptr YContext
}

unbox :: Dynamic -> YExpr
unbox = flip fromDyn (error "Yices1.unbox")

box :: YExpr -> Dynamic
box = toDyn

-- TODO: does this leak solvers?
yices1 :: IO Solver
yices1 = do
  ptr <- c_yices_mk_context
  return $
    let y = Yices1 ptr
    in Solver {
          declare_bool = y1declare_bool y,
          declare_integer = y1declare_integer y,
          declare_bit = y1declare_bit y,
          assert = \p -> withy1 y $ \ctx -> c_yices_assert ctx (unbox p),
         
          bool = \p -> box <$> withy1 y (if p then c_yices_mk_true else c_yices_mk_false),
          integer = \i -> withy1 y $ \ctx -> box <$> c_yices_mk_num ctx (fromInteger i),
          bit = \w v -> withy1 y $ \ctx ->
                let w' = fromInteger w
                    v' = fromInteger v
                in box <$> c_yices_mk_bv_constant ctx w' v',
         
          var = \nm -> withy1 y $ \ctx -> do
             decl <- withCString nm $ c_yices_get_var_decl_from_name ctx
             box <$> c_yices_mk_var_from_decl ctx decl,
         
          ite_bool = ite y,
          ite_integer = ite y,
          ite_bit = ite y,
         
          eq_integer = bprim c_yices_mk_eq y,
          leq_integer = bprim c_yices_mk_le y,
          add_integer = baprim c_yices_mk_sum y,
          sub_integer = baprim c_yices_mk_sub y,
         
          eq_bit = bprim c_yices_mk_eq y,
          leq_bit = bprim c_yices_mk_bv_le y,
          add_bit = bprim c_yices_mk_bv_add y,
          sub_bit = bprim c_yices_mk_bv_sub y,
          mul_bit = bprim c_yices_mk_bv_mul y,
          or_bit = bprim c_yices_mk_bv_or y,

          getBoolValue = y1getBoolValue y,
          getIntegerValue = y1getIntegerValue y,
          getBitVectorValue = y1getBitVectorValue y,
          check = y1check y
       }

y1declare :: String -> Yices1 -> String -> IO ()
y1declare ty y nm = do
    let cmd = "(define " ++ nm ++ " :: " ++ ty ++ ")"
    worked <- withCString cmd $ \str -> do
          withy1 y $ \yctx -> c_yices_parse_command yctx str
    if worked 
       then return ()
       else do
          cstr <- c_yices_get_last_error_message
          msg <- peekCString cstr
          fail $ show msg ++ "\n when running command: \n" ++ cmd

y1declare_bool :: Yices1 -> String -> IO ()
y1declare_bool = y1declare "bool"

y1declare_integer :: Yices1 -> String -> IO ()
y1declare_integer = y1declare "int"

y1declare_bit :: Yices1 -> String -> Integer -> IO ()
y1declare_bit y1 nm w = y1declare ("(bitvector " ++ show w ++ ")") y1 nm

withy1 :: Yices1 -> (Ptr YContext -> IO a) -> IO a
withy1 y f = f (y1_ctx y)

y1check :: Yices1 -> IO Result
y1check y = do
    res <- withy1 y c_yices_check
    return $ toResult res

ite :: Yices1 -> Dynamic -> Dynamic -> Dynamic -> IO Dynamic
ite y p a b = withy1 y $ \ctx -> box <$> c_yices_mk_ite ctx (unbox p) (unbox a) (unbox b)

bprim :: (Ptr YContext -> YExpr -> YExpr -> IO YExpr) ->
         Yices1 -> Dynamic -> Dynamic -> IO Dynamic
bprim f y a b = withy1 y $ \ctx -> box <$> f ctx (unbox a) (unbox b)

baprim :: (Ptr YContext -> Ptr YExpr -> CUInt -> IO YExpr) ->
          Yices1 -> Dynamic -> Dynamic -> IO Dynamic
baprim f y a b = withy1 y $ \ctx ->
    withArray [unbox a, unbox b] $ \arr -> box <$> f ctx arr 2

y1getBoolValue :: Yices1 -> String -> IO Bool
y1getBoolValue y nm = do
    model <- withy1 y c_yices_get_model 
    decl <- withCString nm $ \str ->
                withy1 y $ \yctx -> c_yices_get_var_decl_from_name yctx str
    br <- c_yices_get_value model decl
    case br of
      _ | br == yTrue -> return True
      _ | br == yFalse -> return False
      _ | br == yUndef -> return False

y1getIntegerValue :: Yices1 -> String -> IO Integer
y1getIntegerValue y nm = do
    model <- withy1 y c_yices_get_model 
    decl <- withCString nm $ \str ->
                withy1 y $ \yctx -> c_yices_get_var_decl_from_name yctx str
    x <- alloca $ \ptr -> do
        ir <- c_yices_get_int_value model decl ptr
        if ir == 1
            then peek ptr
            else return 0
    return (toInteger x)

y1getBitVectorValue :: Yices1 -> String -> Integer -> IO Integer
y1getBitVectorValue y nm w = do
    model <- withy1 y c_yices_get_model 
    decl <- withCString nm $ \str ->
                withy1 y $ \yctx -> c_yices_get_var_decl_from_name yctx str
    bits <- allocaArray (fromInteger w) $ \ptr -> do
        ir <- c_yices_get_bitvector_value model decl (fromInteger w) ptr
        if ir == 1
            then peekArray (fromInteger w) ptr
            else return []
    return (bvInteger bits)

bvInteger :: [CInt] -> Integer
bvInteger [] = 0
bvInteger (x:xs) = bvInteger xs * 2 + (fromIntegral x)

