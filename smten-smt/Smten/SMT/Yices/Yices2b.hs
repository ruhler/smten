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
module Smten.SMT.Yices.Yices2b (yices2) where

import Control.Monad.State
import qualified Data.HashMap as Map

import Foreign
import Foreign.C.String
import Foreign.C.Types

import qualified Smten.SMT.Syntax as STX
import Smten.SMT.Yices.FFI2
import qualified Smten.SMT.Yices.Concrete as YC
import qualified Smten.SMT.Solver as S
import Smten.Name
import Smten.Sig
import Smten.Type
import Smten.Bit
import Smten.ExpH
import qualified Smten.HashTable as HT

data Yices2 = Yices2 (Ptr YContext)

-- TODO: this currently leaks context pointers!
-- That should most certainly be fixed somehow.
-- TODO: when do we call c_yices_exit?
yices2 :: IO S.Solver
yices2 = do
  c_yices_init
  ptr <- c_yices_new_context nullPtr
  return $    
    let y2 = Yices2 ptr
    in S.Solver {
          S.push = push y2,
          S.pop = pop y2,
          S.declare = declare y2,
          S.assert = assert y2,
          S.check = check y2,
          S.getIntegerValue = getIntegerValue y2,
          S.getBoolValue = getBoolValue y2,
          S.getBitVectorValue = getBitVectorValue y2
       }

declare :: Yices2 -> Name -> STX.Type -> IO ()
declare _ s ty = do
    ty' <- ytype ty
    term <- c_yices_new_uninterpreted_term ty'
    withCString (unname s) $ c_yices_set_term_name term

assert :: Yices2 -> ExpH -> IO ()
assert (Yices2 yctx) p = do
    p' <- yterm p
    c_yices_assert_formula yctx p'

push :: Yices2 -> IO ()
push (Yices2 yctx) = c_yices_push yctx

pop :: Yices2 -> IO ()
pop (Yices2 yctx) = c_yices_pop yctx

check :: Yices2 -> IO S.Result
check (Yices2 yctx) = do
    st <- c_yices_check_context yctx nullPtr
    return $! fromYSMTStatus st

getIntegerValue :: Yices2 -> Name -> IO Integer
getIntegerValue (Yices2 yctx) nm = do
    model <- c_yices_get_model yctx 1
    x <- alloca $ \ptr -> do
            term <- withCString (unname nm) c_yices_get_term_by_name
            ir <- c_yices_get_int64_value model term ptr
            if ir == 0
               then do 
                  v <- peek ptr
                  return $! v
               else error $ "yices2 get int64 value returned: " ++ show ir
    c_yices_free_model model
    return $! toInteger x

getBoolValue :: Yices2 -> Name -> IO Bool
getBoolValue (Yices2 yctx) nm = do
    model <- c_yices_get_model yctx 1
    x <- alloca $ \ptr -> do
            term <- withCString (unname nm) c_yices_get_term_by_name
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
    
getBitVectorValue :: Yices2 -> Integer -> Name -> IO Integer
getBitVectorValue (Yices2 yctx) w nm = do
    model <- c_yices_get_model yctx 1
    bits <- allocaArray (fromInteger w) $ \ptr -> do
        term <- withCString (unname nm) c_yices_get_term_by_name
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

ytype :: STX.Type -> IO YType
ytype (STX.BitVectorT i) = c_yices_bv_type (fromIntegral i)
ytype (STX.IntegerT) = c_yices_int_type
ytype (STX.BoolT) = c_yices_bool_type
    
type Y2 = StateT (Map.Map EID YTerm) IO

yterm :: ExpH -> IO YTerm
yterm e = evalStateT (def e) Map.empty

use :: ExpH -> Y2 YTerm
use e = do
  m <- get
  case Map.lookup (eid e) m of
      Just v -> return v
      Nothing -> do
        v <- def e
        modify $ Map.insert (eid e) v
        return v

def :: ExpH -> Y2 YTerm
def e
 | Just i <- de_integerEH e = liftIO $ c_yices_int64 (fromInteger i)
 | Just bv <- de_bitEH e = liftIO $ do
     c_yices_bvconst_uint64 (fromInteger (bv_width bv)) (fromInteger (bv_value bv))
 | Just p <- de_boolEH e = liftIO $ if p then c_yices_true else c_yices_false
 | VarEH (Sig nm _) <- force e = liftIO $ withCString (unname nm) c_yices_get_term_by_name
 | PrimEH n t _ xs <- force e = do
      case HT.lookup n prims of
        Just f -> f t xs
        Nothing -> error $ "yices2: primitive not supported: " ++ unname n
 | IfEH _ p a b <- force e = do
     [p', a', b'] <- mapM use [p, a, b]
     liftIO $ c_yices_ite p' a' b'

prims :: HT.HashTable Name (Type -> [ExpH] -> Y2 YTerm)
prims = HT.table [
    (name "Prelude.__prim_eq_Integer", bprim c_yices_eq),
    (name "Prelude.__prim_add_Integer", bprim c_yices_add),
    (name "Prelude.__prim_sub_Integer", bprim c_yices_sub),
    (name "Prelude.__prim_mul_Integer", bprim c_yices_mul),
    (name "Prelude.__prim_lt_Integer", bprim c_yices_arith_lt_atom),
    (name "Prelude.__prim_leq_Integer", bprim c_yices_arith_leq_atom),
    (name "Prelude.__prim_gt_Integer", bprim c_yices_arith_gt_atom),
    (name "Prelude.__prim_geq_Integer", bprim c_yices_arith_geq_atom),
    (name "Smten.Bit.__prim_eq_Bit", bprim c_yices_eq),
    (name "Smten.Bit.__prim_lt_Bit", bprim c_yices_bvlt_atom),
    (name "Smten.Bit.__prim_leq_Bit", bprim c_yices_bvle_atom),
    (name "Smten.Bit.__prim_gt_Bit", bprim c_yices_bvgt_atom),
    (name "Smten.Bit.__prim_geq_Bit", bprim c_yices_bvge_atom),
    (name "Smten.Bit.__prim_add_Bit", bprim c_yices_bvadd),
    (name "Smten.Bit.__prim_sub_Bit", bprim c_yices_bvsub),
    (name "Smten.Bit.__prim_mul_Bit", bprim c_yices_bvmul),
    (name "Smten.Bit.__prim_or_Bit", bprim c_yices_bvor),
    (name "Smten.Bit.__prim_and_Bit", bprim c_yices_bvand),
    (name "Smten.Bit.__prim_not_Bit", uprim c_yices_bvnot),
    (name "Smten.Bit.__prim_concat_Bit", bprim c_yices_bvconcat),
    (name "Smten.Bit.__prim_shl_Bit", bprim c_yices_bvshl),
    (name "Smten.Bit.__prim_lshr_Bit", bprim c_yices_bvlshr),
    (name "Smten.Bit.__prim_zeroExtend_Bit", primZeroExtend),
    (name "Smten.Bit.__prim_signExtend_Bit", primSignExtend),
    (name "Smten.Bit.__prim_truncate_Bit", primTruncate),
    (name "Smten.Bit.__prim_extract_Bit", primExtract)
    ]

uprim :: (YTerm -> IO YTerm) -> Type -> [ExpH] -> Y2 YTerm
uprim f _ [a] = do
    a' <- use a
    liftIO $ f a'

bprim :: (YTerm -> YTerm -> IO YTerm) -> Type -> [ExpH] -> Y2 YTerm
bprim f _ [a, b] = do
    a' <- use a
    b' <- use b
    liftIO $ f a' b'

primZeroExtend :: Type -> [ExpH] -> Y2 YTerm
primZeroExtend bt [a] = do
    let Just sw = de_bitT (typeof $ force a)
        Just tw = de_bitT bt
        n = tw - sw
    a' <- use a
    liftIO $ c_yices_zero_extend a' (fromInteger n)

primSignExtend :: Type -> [ExpH] -> Y2 YTerm
primSignExtend bt [a] = do
    let Just sw = de_bitT (typeof (force a))
        Just tw = de_bitT bt
        n = tw - sw
    a' <- use a
    liftIO $ c_yices_sign_extend a' (fromInteger n)

primExtract :: Type -> [ExpH] -> Y2 YTerm
primExtract t [x, li]= do
    let Just i = de_integerEH x
        Just tw = de_bitT t
        begin = i + tw - 1
        end = i
    x' <- use x
    liftIO $ c_yices_bvextract x' (fromInteger begin) (fromInteger end)

primTruncate :: Type -> [ExpH] -> Y2 YTerm
primTruncate t [x] = do
    let Just (_, bt) = de_arrowT t
        Just tw = de_bitT bt
        begin = tw - 1
        end = 0
    x' <- use x
    liftIO $ c_yices_bvextract x' (fromInteger begin) (fromInteger end)
    
