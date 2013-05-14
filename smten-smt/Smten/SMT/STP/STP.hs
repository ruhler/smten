
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.SMT.STP.STP (stp) where

import Data.Functor
import Data.List(genericLength)
import Data.IORef
import qualified Data.Map as Map

import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Foreign.Concurrent as F
import Smten.SMT.STP.FFI

import Smten.SMT.AST
import qualified Smten.SMT.Assert as A
import qualified Smten.SMT.Solver as S
import Smten.Name
import Smten.Lit
import Smten.Bit
import Smten.Type
import Smten.Sig
import Smten.ExpH
import qualified Smten.HashTable as HT

data STP = STP {
    stp_fvc :: ForeignPtr STP_VC,
    stp_vars :: IORef (Map.Map Name (Ptr STP_Expr)),
    stp_nid :: IORef Integer
}

withvc :: STP -> (Ptr STP_VC -> IO a) -> IO a
withvc s = withForeignPtr (stp_fvc s)

mkType :: STP -> Type -> IO (Ptr STP_Type)
mkType s t
 | t == boolT = withvc s c_vc_boolType
 | Just w <- de_bitT t = withvc s $ \vc -> c_vc_bvType vc (fromInteger w)
 | t == integerT = error $ "STP does not support Integer type"

instance AST STP (Ptr STP_Expr) where
  fresh = stpfresh
  assert s e = withvc s $ \vc -> c_vc_assertFormula vc e
 
  literal s l
    | Just i <- de_integerL l = error $ "STP does not support integer literals"
    | Just bv <- de_bitL l = withvc s $ \vc ->
        let w = fromInteger $ bv_width bv
            v = fromInteger $ bv_value bv
        in c_vc_bvConstExprFromLL vc w v

  bool s True = withvc s c_vc_trueExpr
  bool s False = withvc s c_vc_falseExpr

  var s nm = do
    vars <- readIORef (stp_vars s)
    case Map.lookup nm vars of
        Just v -> return v
        Nothing -> error $ "STP: unknown var: " ++ unname nm

  ite s p a b = withvc s $ \vc -> c_vc_iteExpr vc p a b

  unary = HT.table [(name "Smten.Bit.__prim_not_Bit", \s x ->
                        withvc s $ \vc -> c_vc_bvNotExpr vc x)]

  binary = 
    let bprim :: (Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr))
              -> STP -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)
        bprim f s a b = withvc s $ \vc -> f vc a b

        blprim :: (Ptr STP_VC -> CInt -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr))
               -> STP -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)
        blprim f s a b = withvc s $ \vc -> do
            n <- c_vc_getBVLength vc a
            f vc n a b

    in HT.table [
     (name "Smten.Bit.__prim_eq_Bit", bprim c_vc_eqExpr),
     (name "Smten.Bit.__prim_lt_Bit", bprim c_vc_bvLtExpr),
     (name "Smten.Bit.__prim_leq_Bit", bprim c_vc_bvLeExpr),
     (name "Smten.Bit.__prim_gt_Bit", bprim c_vc_bvGtExpr),
     (name "Smten.Bit.__prim_geq_Bit", bprim c_vc_bvGeExpr),
     (name "Smten.Bit.__prim_add_Bit", blprim c_vc_bvPlusExpr),
     (name "Smten.Bit.__prim_sub_Bit", blprim c_vc_bvMinusExpr),
     (name "Smten.Bit.__prim_or_Bit", bprim c_vc_orExpr),
     (name "Smten.Bit.__prim_and_Bit", bprim c_vc_andExpr),
     (name "Smten.Bit.__prim_concat_Bit", bprim c_vc_bvConcatExpr),
     (name "Smten.Bit.__prim_shl_Bit", blprim c_vc_bvLeftShiftExprExpr),
     (name "Smten.Bit.__prim_lshr_Bit", blprim c_vc_bvRightShiftExprExpr)
    ]

  zeroextend s x n = withvc s $ \vc -> do
    zeros <- c_vc_bvConstExprFromLL vc (fromInteger n) 0
    c_vc_bvConcatExpr vc zeros x

  signextend s x n = withvc s $ \vc -> do
    c_vc_bvSignExtend vc x (fromInteger n)

  extract = error "TODO: STP extract"
  truncate = error "TODO: STP truncate"


stp :: IO S.Solver
stp = do
  ptr <- c_vc_createValidityChecker
  -- TODO: this leaks STP pointers?
  fvc <- F.newForeignPtr ptr (return ())
  vars <- newIORef Map.empty
  nid <- newIORef 0
  let s = STP { stp_fvc = fvc, stp_vars = vars, stp_nid = nid }
  return $ S.Solver {
       S.push = push s,
       S.pop = pop s,
       S.fresh = stpfresh s,
       S.assert = stpassert s,
       S.check = check s,
       S.getIntegerValue = getIntegerValue s,
       S.getBoolValue = getBoolValue s,
       S.getBitVectorValue = getBitVectorValue s
    }
        
stpfresh :: STP -> Type -> IO Name
stpfresh s t = do
    nid <- readIORef (stp_nid s)
    modifyIORef' (stp_nid s) (+ 1)
    let nm = name $ "f~" ++ show nid
    st <- mkType s t        
    v <- withvc s $ \vc -> (withCString (unname nm) $ \cnm -> c_vc_varExpr vc cnm st)
    modifyIORef (stp_vars s) $ Map.insert nm v
    return nm

stpassert :: STP -> ExpH -> IO ()
stpassert = A.assert

push :: STP -> IO ()
push s = withvc s c_vc_push

pop :: STP -> IO ()
pop s = withvc s c_vc_pop

-- To check for satisfiability, we query if False is valid. If False is
-- valid, the assertions imply False, meaning they are unsatisfiable. If
-- False is not valid, then there's some assignment which satisfies all
-- the assertions.
check :: STP -> IO S.Result
check s = do
    false <- withvc s c_vc_falseExpr
    r <- withvc s $ \vc -> c_vc_query vc false
    case r of
        0 -> return S.Satisfiable     -- False is INVALID
        1 -> return S.Unsatisfiable   -- False is VALID
        _ -> error $ "STP.check: vc_query returned " ++ show r
    
getIntegerValue :: STP -> Name -> IO Integer
getIntegerValue _ _ = error $ "STP does not support free Integers"

getBoolValue :: STP -> Name -> IO Bool
getBoolValue s nm = do
    v <- var s nm
    val <- withvc s $ \vc -> c_vc_getCounterExample vc v
    b <- c_vc_isBool val
    case b of
        0 -> return False
        1 -> return True
        x -> error $ "STP.getBoolValue got value " ++ show x ++ " for " ++ unname nm
    
getBitVectorValue :: STP -> Integer -> Name -> IO Integer
getBitVectorValue s _ nm = do
    v <- var s nm
    val <- withvc s $ \vc -> c_vc_getCounterExample vc v
    fromIntegral <$> c_getBVUnsignedLongLong val
    
