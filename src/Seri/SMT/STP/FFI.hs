
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Seri.SMT.STP.FFI (
    STP_VC, STP_Type, STP_Expr,
    c_vc_createValidityChecker,
    c_vc_Destroy,

    c_vc_boolType,
    c_vc_bvType,

    c_vc_varExpr,
    c_vc_assertFormula,
    c_vc_query,

    c_vc_trueExpr,
    c_vc_falseExpr,
    c_vc_notExpr,
    c_vc_orExpr,
    c_vc_andExpr,
    c_vc_eqExpr,

    c_vc_bvConstExprFromLL,
    c_vc_bvPlusExpr,
    c_vc_bvOrExpr,
    c_vc_bvAndExpr,
    c_vc_bvLeftShiftExpr,

    c_vc_getCounterExample,
    c_vc_isBool,
    c_vc_getBVLength,
    ) where

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

foreign import ccall "vc_andExpr"
    c_vc_andExpr  :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_eqExpr"
    c_vc_eqExpr  :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvConstExprFromLL"
    c_vc_bvConstExprFromLL  :: Ptr STP_VC -> CInt -> CULLong -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvPlusExpr"
    c_vc_bvPlusExpr  :: Ptr STP_VC -> CInt -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvOrExpr"
    c_vc_bvOrExpr  :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvAndExpr"
    c_vc_bvAndExpr  :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_bvLeftShiftExpr"
    c_vc_bvLeftShiftExpr  :: Ptr STP_VC -> CInt -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_getCounterExample"
    c_vc_getCounterExample  :: Ptr STP_VC -> Ptr STP_Expr -> IO (Ptr STP_Expr)

foreign import ccall "vc_isBool"
    c_vc_isBool  :: Ptr STP_Expr -> IO CInt

foreign import ccall "vc_getBVLength"
    c_vc_getBVLength  :: Ptr STP_VC -> Ptr STP_Expr -> IO CInt

