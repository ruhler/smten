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

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls  #-}

-- | Backend for the Yices1 Solver
--
-- NOTE: This assumes all the symbols from the yices1 library starting with
-- yices_ have been renamed to yices1_. This is so yices1 and yices2 can
-- coexist.
module Seri.SMT.Yices.Yices1 (yices1) where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Foreign.Concurrent as F

import qualified Seri.SMT.Solver as S
import Seri.SMT.Syntax
import qualified Seri.SMT.Yices.Concrete as YC

import qualified Seri.SMT.Query as Q

data YContext
data YModel
data YDecl
data Yices1 = Yices1 (ForeignPtr YContext)

type YBool = CInt

yFalse, yTrue, yUndef :: YBool
yFalse = -1
yTrue  = 1
yUndef = 0


foreign import ccall "yices1_mk_context"
    c_yices_mk_context  :: IO (Ptr YContext)

foreign import ccall "yices1_del_context"
    c_yices_del_context :: Ptr YContext -> IO ()

foreign import ccall "yices1_parse_command"
    c_yices_parse_command :: Ptr YContext -> CString -> IO Bool

foreign import ccall "yices1_check"
    c_yices_check :: Ptr YContext -> IO YBool

foreign import ccall "yices1_get_model"
    c_yices_get_model :: Ptr YContext -> IO (Ptr YModel)

foreign import ccall "yices1_display_model"
    c_yices_display_model :: Ptr YModel -> IO ()

foreign import ccall "yices1_enable_type_checker"
    c_yices_enable_type_checker :: Bool -> IO ()

foreign import ccall "yices1_get_last_error_message"
    c_yices_get_last_error_message :: IO CString

foreign import ccall "yices1_get_value"
    c_yices_get_value :: Ptr YModel -> Ptr YDecl -> IO YBool

foreign import ccall "yices1_get_int_value"
    c_yices_get_int_value :: Ptr YModel -> Ptr YDecl -> Ptr CLong -> IO CInt

foreign import ccall "yices1_get_bitvector_value"
    c_yices_get_bitvector_value :: Ptr YModel -> Ptr YDecl -> CUInt -> Ptr CInt -> IO CInt

foreign import ccall "yices1_get_var_decl_from_name"
    c_yices_get_var_decl_from_name :: Ptr YContext -> CString -> IO (Ptr YDecl)
                          

toResult :: YBool -> S.Result
toResult n
    | n == yFalse = S.Unsatisfiable
    | n == yTrue  = S.Satisfiable
    | otherwise   = S.Undefined

run :: Yices1 -> Command -> IO ()
run (Yices1 fp) cmd = do
    worked <- withCString (YC.concrete cmd) $ \str -> do
          withForeignPtr fp $ \yctx ->
            c_yices_parse_command yctx str
    if worked 
       then return ()
       else do
          cstr <- c_yices_get_last_error_message
          msg <- peekCString cstr
          fail $ show msg
                    ++ "\n when running command: \n" 
                    ++ YC.pretty cmd

check :: Yices1 -> IO S.Result
check (Yices1 fp) = do
    res <- withForeignPtr fp c_yices_check
    return $ toResult res

getIntegerValue :: Yices1 -> String -> IO Integer
getIntegerValue (Yices1 fp) nm = do
    model <- withForeignPtr fp c_yices_get_model 
    decl <- withCString nm $ \str ->
                withForeignPtr fp $ \yctx ->
                    c_yices_get_var_decl_from_name yctx str
    x <- alloca $ \ptr -> do
        ir <- c_yices_get_int_value model decl ptr
        if ir == 1
            then peek ptr
            else return 0
    return (toInteger x)

getBoolValue :: Yices1 -> String -> IO Bool
getBoolValue (Yices1 fp) nm = do
    model <- withForeignPtr fp c_yices_get_model 
    decl <- withCString nm $ \str ->
                withForeignPtr fp $ \yctx ->
                    c_yices_get_var_decl_from_name yctx str
    br <- c_yices_get_value model decl
    case br of
      _ | br == yTrue -> return True
      _ | br == yFalse -> return False
      _ | br == yUndef -> return False

getBitVectorValue :: Yices1 -> Integer -> String -> IO Integer
getBitVectorValue (Yices1 fp) w nm = do
    model <- withForeignPtr fp c_yices_get_model 
    decl <- withCString nm $ \str ->
                withForeignPtr fp $ \yctx ->
                    c_yices_get_var_decl_from_name yctx str
    bits <- allocaArray (fromInteger w) $ \ptr -> do
        ir <- c_yices_get_bitvector_value model decl (fromInteger w) ptr
        if ir == 1
            then peekArray (fromInteger w) ptr
            else return []
    return (bvInteger bits)

bvInteger :: [CInt] -> Integer
bvInteger [] = 0
bvInteger (x:xs) = bvInteger xs * 2 + (fromIntegral x)
        
yices1 :: IO S.Solver
yices1 = do
  c_yices_enable_type_checker True
  ptr <- c_yices_mk_context
  fp  <- F.newForeignPtr ptr (c_yices_del_context ptr)
  return $
    let y1 = Yices1 fp
    in S.Solver {
          S.pretty = YC.pretty,
          S.run = run y1,
          S.check = check y1,
          S.getIntegerValue = getIntegerValue y1,
          S.getBoolValue = getBoolValue y1,
          S.getBitVectorValue = getBitVectorValue y1
       }
