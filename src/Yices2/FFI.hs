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

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Raw FFI interface to yices2.
module Yices2.FFI (
    YContext, YContextConfig, YParam, YType, YTerm,
    SMTStatus(..), toYSMTStatus, fromYSMTStatus,
    c_yices_init,
    c_yices_exit,
    c_yices_print_error,
    c_yices_new_scalar_type,
    c_yices_constant,
    c_yices_new_uninterpreted_type,
    c_yices_new_uninterpreted_term,
    c_yices_set_type_name,
    c_yices_set_term_name,
    c_yices_parse_type,
    c_yices_parse_term,

    -- Term constructors
    c_yices_true,
    c_yices_false,
    c_yices_rational64,
    c_yices_eq,
    c_yices_tuple,
    c_yices_tuple_update,
    c_yices_select,
    c_yices_application,
    c_yices_update,
    c_yices_ite,
    c_yices_arith_lt_atom,
    c_yices_arith_gt_atom,
    c_yices_add,
    c_yices_sub,
    c_yices_mul,
    c_yices_or2,
    c_yices_and2,
    c_yices_xor2,
    c_yices_and,

    -- Working with contexts
    c_yices_new_context,
    c_yices_free_context,
    c_yices_assert_formula,
    c_yices_check_context,
    c_yices_push,
    c_yices_pop,
    c_yices_get_model,
    c_yices_free_model,
    c_yices_get_int64_value,
    c_fdopen,
    ) where

import Data.Int

import Foreign
import Foreign.C.String
import Foreign.C.Types

data YContext
data YModel
data YContextConfig
data YParam
type YType = Int32
type YTerm = Int32
type YSMTStatus = CInt

data SMTStatus = 
    STATUS_IDLE
  | STATUS_SEARCHING
  | STATUS_UNKNOWN
  | STATUS_SAT
  | STATUS_UNSAT
  | STATUS_INTERRUPTED
  | STATUS_ERROR
    deriving(Eq, Show)

toYSMTStatus :: SMTStatus -> YSMTStatus
toYSMTStatus STATUS_IDLE = 0
toYSMTStatus STATUS_SEARCHING = 1
toYSMTStatus STATUS_UNKNOWN = 2
toYSMTStatus STATUS_SAT = 3
toYSMTStatus STATUS_UNSAT = 4
toYSMTStatus STATUS_INTERRUPTED = 5
toYSMTStatus STATUS_ERROR = 6

fromYSMTStatus :: YSMTStatus -> SMTStatus
fromYSMTStatus 0 = STATUS_IDLE       
fromYSMTStatus 1 = STATUS_SEARCHING  
fromYSMTStatus 2 = STATUS_UNKNOWN    
fromYSMTStatus 3 = STATUS_SAT        
fromYSMTStatus 4 = STATUS_UNSAT      
fromYSMTStatus 5 = STATUS_INTERRUPTED
fromYSMTStatus 6 = STATUS_ERROR      
 

foreign import ccall "yices_init"
    c_yices_init :: IO ()

foreign import ccall "yices_exit"
    c_yices_exit :: IO ()

foreign import ccall "yices_print_error"
    c_yices_print_error :: Ptr CFile -> IO ()

foreign import ccall "yices_new_scalar_type"
    c_yices_new_scalar_type :: Int32 -> IO YType

foreign import ccall "yices_constant"
    c_yices_constant :: YType -> Int32 -> IO YTerm

foreign import ccall "yices_new_uninterpreted_type"
    c_yices_new_uninterpreted_type :: IO YType

foreign import ccall "yices_new_uninterpreted_term"
    c_yices_new_uninterpreted_term :: YType -> IO YTerm

foreign import ccall "yices_set_type_name"
    c_yices_set_type_name :: YType -> CString -> IO ()

foreign import ccall "yices_set_term_name"
    c_yices_set_term_name :: YTerm -> CString -> IO ()

foreign import ccall "yices_parse_type"
    c_yices_parse_type :: CString -> IO YType

foreign import ccall "yices_parse_term"
    c_yices_parse_term :: CString -> IO YTerm


foreign import ccall "yices_true"
    c_yices_true ::  IO YTerm

foreign import ccall "yices_false"
    c_yices_false ::  IO YTerm

foreign import ccall "yices_rational64"
    c_yices_rational64 ::  Int64 -> Word64 -> IO YTerm

foreign import ccall "yices_eq"
    c_yices_eq :: YTerm -> YTerm -> IO YTerm

foreign import ccall "yices_tuple"
    c_yices_tuple :: Word32 -> Ptr YTerm -> IO YTerm

foreign import ccall "yices_tuple_update"
    c_yices_tuple_update :: YTerm -> Word32 -> YTerm -> IO YTerm

foreign import ccall "yices_select"
    c_yices_select :: Word32 -> YTerm -> IO YTerm

foreign import ccall "yices_application"
    c_yices_application :: YTerm -> Word32 -> Ptr YTerm -> IO YTerm

foreign import ccall "yices_update"
    c_yices_update :: YTerm -> Word32 -> Ptr YTerm -> YTerm -> IO YTerm

foreign import ccall "yices_ite"
    c_yices_ite :: YTerm -> YTerm -> YTerm -> IO YTerm

foreign import ccall "yices_arith_lt_atom"
    c_yices_arith_lt_atom :: YTerm -> YTerm -> IO YTerm

foreign import ccall "yices_arith_gt_atom"
    c_yices_arith_gt_atom :: YTerm -> YTerm -> IO YTerm

foreign import ccall "yices_add"
    c_yices_add :: YTerm -> YTerm -> IO YTerm

foreign import ccall "yices_sub"
    c_yices_sub :: YTerm -> YTerm -> IO YTerm

foreign import ccall "yices_mul"
    c_yices_mul :: YTerm -> YTerm -> IO YTerm

foreign import ccall "yices_or2"
    c_yices_or2 :: YTerm -> YTerm -> IO YTerm

foreign import ccall "yices_and2"
    c_yices_and2 :: YTerm -> YTerm -> IO YTerm

foreign import ccall "yices_and"
    c_yices_and :: Word32 -> Ptr YTerm -> IO YTerm

foreign import ccall "yices_xor2"
    c_yices_xor2 :: YTerm -> YTerm -> IO YTerm

foreign import ccall "yices_new_context"
    c_yices_new_context :: Ptr YContextConfig -> IO (Ptr YContext)

foreign import ccall "yices_free_context"
    c_yices_free_context :: Ptr YContext -> IO ()

foreign import ccall "yices_assert_formula"
    c_yices_assert_formula :: Ptr YContext -> YTerm -> IO ()

foreign import ccall "yices_check_context"
    c_yices_check_context :: Ptr YContext -> Ptr YParam -> IO YSMTStatus

foreign import ccall "yices_push"
    c_yices_push :: Ptr YContext -> IO ()

foreign import ccall "yices_pop"
    c_yices_pop :: Ptr YContext -> IO ()

foreign import ccall "yices_get_model"
    c_yices_get_model :: Ptr YContext -> Int32 -> IO (Ptr YModel)

foreign import ccall "yices_free_model"
    c_yices_free_model :: Ptr YModel -> IO ()

foreign import ccall "yices_get_int64_value"
    c_yices_get_int64_value :: Ptr YModel -> YTerm -> Ptr Int64 -> IO Int32

foreign import ccall "fdopen"
    c_fdopen :: CInt -> CString -> IO (Ptr CFile)

