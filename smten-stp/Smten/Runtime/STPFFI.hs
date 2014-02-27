
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fprof-auto-top #-}

module Smten.Runtime.STPFFI (
    STP_VC, STP_Type, STP_Expr,
    c_vc_createValidityChecker,
    c_vc_Destroy,
    c_vc_DeleteExpr,
    c_vc_boolType,
    c_vc_bvType,
    c_vc_varExpr,
    c_vc_assertFormula,
    c_vc_push,
    c_vc_pop,
    c_vc_query,
    c_vc_trueExpr,
    c_vc_falseExpr,
    c_vc_notExpr,
    c_vc_orExpr,
    c_vc_orExprN,
    c_vc_andExpr,
    c_vc_andExprN,
    c_vc_eqExpr,
    c_vc_iteExpr,
    c_vc_bvConstExprFromLL,
    c_vc_bvConstExprFromDecStr,
    c_vc_bvPlusExpr,
    c_vc_bvMinusExpr,
    c_vc_bvOrExpr,
    c_vc_bvAndExpr,
    c_vc_bvNotExpr,
    c_vc_bvLeftShiftExpr,
    c_vc_bvLeftShiftExprExpr,
    c_vc_bvRightShiftExpr,
    c_vc_bvRightShiftExprExpr,
    c_vc_bvConcatExpr,
    c_vc_bvSignExtend,
    c_vc_bvExtract,
    c_vc_getCounterExample,
    c_vc_isBool,
    c_vc_getBVLength,
    c_getBVUnsignedLongLong,
    c_vc_bvLtExpr,
    c_vc_bvLeExpr,
    c_vc_bvGtExpr,
    c_vc_bvGeExpr,
    ) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

data STP_VC_
type STP_VC = Ptr STP_VC_


data STP_Expr_
type STP_Expr = Ptr STP_Expr_
type STP_Type = STP_Expr

foreign import ccall "vc_createValidityChecker"
    c_vc_createValidityChecker  :: IO STP_VC

foreign import ccall "vc_Destroy"
    c_vc_Destroy  :: STP_VC -> IO ()

foreign import ccall "vc_DeleteExpr"
    c_vc_DeleteExpr  :: STP_Expr -> IO ()

foreign import ccall "vc_boolType"
    c_vc_boolType  :: STP_VC -> IO STP_Type

foreign import ccall "vc_bvType"
    c_vc_bvType  :: STP_VC -> CInt -> IO STP_Type

foreign import ccall "vc_varExpr"
    c_vc_varExpr  :: STP_VC -> CString -> STP_Type -> IO STP_Expr

foreign import ccall "vc_assertFormula"
    c_vc_assertFormula  :: STP_VC -> STP_Expr -> IO ()

foreign import ccall "vc_push"
    c_vc_push  :: STP_VC -> IO ()

foreign import ccall "vc_pop"
    c_vc_pop  :: STP_VC -> IO ()

foreign import ccall "vc_query"
    c_vc_query  :: STP_VC -> STP_Expr -> IO CInt


foreign import ccall "vc_trueExpr"
    c_vc_trueExpr  :: STP_VC -> IO STP_Expr

foreign import ccall "vc_falseExpr"
    c_vc_falseExpr  :: STP_VC -> IO STP_Expr

foreign import ccall "vc_notExpr"
    c_vc_notExpr  :: STP_VC -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_orExpr"
    c_vc_orExpr  :: STP_VC -> STP_Expr -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_orExprN"
    c_vc_orExprN  :: STP_VC -> Ptr STP_Expr -> CInt -> IO STP_Expr

foreign import ccall "vc_andExpr"
    c_vc_andExpr  :: STP_VC -> STP_Expr -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_andExprN"
    c_vc_andExprN  :: STP_VC -> Ptr STP_Expr -> CInt -> IO STP_Expr

foreign import ccall "vc_eqExpr"
    c_vc_eqExpr  :: STP_VC -> STP_Expr -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_iteExpr"
    c_vc_iteExpr  :: STP_VC -> STP_Expr -> STP_Expr -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_bvConstExprFromLL"
    c_vc_bvConstExprFromLL  :: STP_VC -> CInt -> CULLong -> IO STP_Expr

foreign import ccall "vc_bvConstExprFromDecStr"
    c_vc_bvConstExprFromDecStr  :: STP_VC -> CInt -> CString -> IO STP_Expr

foreign import ccall "vc_bvPlusExpr"
    c_vc_bvPlusExpr  :: STP_VC -> CInt -> STP_Expr -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_bvMinusExpr"
    c_vc_bvMinusExpr  :: STP_VC -> CInt -> STP_Expr -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_bvOrExpr"
    c_vc_bvOrExpr  :: STP_VC -> STP_Expr -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_bvAndExpr"
    c_vc_bvAndExpr  :: STP_VC -> STP_Expr -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_bvNotExpr"
    c_vc_bvNotExpr  :: STP_VC -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_bvLeftShiftExpr"
    c_vc_bvLeftShiftExpr  :: STP_VC -> CInt -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_bvLeftShiftExprExpr"
    c_vc_bvLeftShiftExprExpr  :: STP_VC -> CInt -> STP_Expr -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_bvRightShiftExpr"
    c_vc_bvRightShiftExpr  :: STP_VC -> CInt -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_bvRightShiftExprExpr"
    c_vc_bvRightShiftExprExpr  :: STP_VC -> CInt -> STP_Expr -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_bvConcatExpr"
    c_vc_bvConcatExpr  :: STP_VC -> STP_Expr -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_bvSignExtend"
    c_vc_bvSignExtend  :: STP_VC -> STP_Expr -> CInt -> IO STP_Expr

foreign import ccall "vc_bvExtract"
    c_vc_bvExtract  :: STP_VC -> STP_Expr -> CInt -> CInt -> IO STP_Expr

foreign import ccall "vc_getCounterExample"
    c_vc_getCounterExample  :: STP_VC -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_isBool"
    c_vc_isBool  :: STP_Expr -> IO CInt

foreign import ccall "vc_getBVLength"
    c_vc_getBVLength  :: STP_VC -> STP_Expr -> IO CInt

foreign import ccall "getBVUnsignedLongLong"
    c_getBVUnsignedLongLong  :: STP_Expr -> IO CULLong

foreign import ccall "vc_bvLtExpr"
    c_vc_bvLtExpr  :: STP_VC -> STP_Expr -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_bvLeExpr"
    c_vc_bvLeExpr  :: STP_VC -> STP_Expr -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_bvGtExpr"
    c_vc_bvGtExpr  :: STP_VC -> STP_Expr -> STP_Expr -> IO STP_Expr

foreign import ccall "vc_bvGeExpr"
    c_vc_bvGeExpr  :: STP_VC -> STP_Expr -> STP_Expr -> IO STP_Expr
