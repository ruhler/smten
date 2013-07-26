
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Smten.Runtime.STPFFI where

import Foreign
import Foreign.C.String
import Foreign.C.Types

data STP_VC
data STP_Type
data STP_Expr

foreign import ccall "vc_createValidityChecker"
    c_vc_createValidityChecker  :: IO (Ptr STP_VC)

foreign import ccall "vc_Destroy"
    c_vc_Destroy  :: Ptr STP_VC -> IO ()

foreign import ccall "vc_boolType"
    c_vc_boolType  :: Ptr STP_VC -> IO (Ptr STP_Type)

foreign import ccall "vc_bvType"
    c_vc_bvType  :: Ptr STP_VC -> CInt -> IO (Ptr STP_Type)

foreign import ccall "vc_varExpr"
    c_vc_varExpr  :: Ptr STP_VC -> CString -> Ptr STP_Type -> IO (Ptr STP_Expr)

foreign import ccall "vc_assertFormula"
    c_vc_assertFormula  :: Ptr STP_VC -> Ptr STP_Expr -> IO ()

foreign import ccall "vc_push"
    c_vc_push  :: Ptr STP_VC -> IO ()

foreign import ccall "vc_pop"
    c_vc_pop  :: Ptr STP_VC -> IO ()

foreign import ccall "vc_query"
    c_vc_query  :: Ptr STP_VC -> Ptr STP_Expr -> IO CInt


foreign import ccall "vc_trueExpr"
    c_vc_trueExpr  :: Ptr STP_VC -> IO (Ptr STP_Expr)

foreign import ccall "vc_falseExpr"
    c_vc_falseExpr  :: Ptr STP_VC -> IO (Ptr STP_Expr)

foreign import ccall "vc_notExpr"
    c_vc_notExpr  :: Ptr STP_VC -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_orExpr"
    c_vc_orExpr  :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_orExprN"
    c_vc_orExprN  :: Ptr STP_VC -> Ptr (Ptr STP_Expr) -> CInt -> IO (Ptr STP_Expr)

foreign import ccall "vc_andExpr"
    c_vc_andExpr  :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_andExprN"
    c_vc_andExprN  :: Ptr STP_VC -> Ptr (Ptr STP_Expr) -> CInt -> IO (Ptr STP_Expr)

foreign import ccall "vc_eqExpr"
    c_vc_eqExpr  :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_iteExpr"
    c_vc_iteExpr  :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvConstExprFromLL"
    c_vc_bvConstExprFromLL  :: Ptr STP_VC -> CInt -> CULLong -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvPlusExpr"
    c_vc_bvPlusExpr  :: Ptr STP_VC -> CInt -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvMinusExpr"
    c_vc_bvMinusExpr  :: Ptr STP_VC -> CInt -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvOrExpr"
    c_vc_bvOrExpr  :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvAndExpr"
    c_vc_bvAndExpr  :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvNotExpr"
    c_vc_bvNotExpr  :: Ptr STP_VC -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvLeftShiftExpr"
    c_vc_bvLeftShiftExpr  :: Ptr STP_VC -> CInt -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvLeftShiftExprExpr"
    c_vc_bvLeftShiftExprExpr  :: Ptr STP_VC -> CInt -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvRightShiftExprExpr"
    c_vc_bvRightShiftExprExpr  :: Ptr STP_VC -> CInt -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvConcatExpr"
    c_vc_bvConcatExpr  :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvSignExtend"
    c_vc_bvSignExtend  :: Ptr STP_VC -> Ptr STP_Expr -> CInt -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvExtract"
    c_vc_bvExtract  :: Ptr STP_VC -> Ptr STP_Expr -> CInt -> CInt -> IO (Ptr STP_Expr)

foreign import ccall "vc_getCounterExample"
    c_vc_getCounterExample  :: Ptr STP_VC -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_isBool"
    c_vc_isBool  :: Ptr STP_Expr -> IO CInt

foreign import ccall "vc_getBVLength"
    c_vc_getBVLength  :: Ptr STP_VC -> Ptr STP_Expr -> IO CInt

foreign import ccall "getBVUnsignedLongLong"
    c_getBVUnsignedLongLong  :: Ptr STP_Expr -> IO CULLong

foreign import ccall "vc_bvLtExpr"
    c_vc_bvLtExpr  :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvLeExpr"
    c_vc_bvLeExpr  :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvGtExpr"
    c_vc_bvGtExpr  :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvGeExpr"
    c_vc_bvGeExpr  :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)
