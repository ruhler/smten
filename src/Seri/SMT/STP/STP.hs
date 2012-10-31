
module Seri.SMT.STP.STP (STP(), stp) where

import Data.Functor
import Data.IORef
import qualified Data.Map as Map

import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Foreign.Concurrent as F
import Seri.SMT.STP.FFI

import qualified Seri.Lambda as S
import qualified Seri.SMT.Query as Q
import Seri.SMT.Solver
import Seri.SMT.Syntax

import qualified Seri.SMT.STP.Concrete as C

data STP = STP {
    stp_fvc :: ForeignPtr STP_VC,
    stp_vars :: IORef (Map.Map Symbol (Ptr STP_Expr))
}

withvc :: STP -> (Ptr STP_VC -> IO a) -> IO a
withvc s = withForeignPtr (stp_fvc s)

mkType :: STP -> Type -> IO (Ptr STP_Type)
mkType s BoolT = withvc s c_vc_boolType
mkType s (BitVectorT w) = withvc s $ \vc -> c_vc_bvType vc (fromInteger w)
mkType _ IntegerT = error $ "STP does not support Integer type"
mkType _ (ArrowT _) = error $ "STP does not support Function type"

mkExpr :: STP -> Expression -> IO (Ptr STP_Expr)
mkExpr s (LitE (BoolL True)) = withvc s c_vc_trueExpr
mkExpr s (LitE (BoolL False)) = withvc s c_vc_falseExpr
mkExpr s (AppE (VarE "not") [a]) = do
    ae <- mkExpr s a
    withvc s $ \vc -> c_vc_notExpr vc ae
mkExpr s (AppE (VarE "or") [a, b]) = do
    ae <- mkExpr s a
    be <- mkExpr s b
    withvc s $ \vc -> c_vc_orExpr vc ae be
mkExpr s (AppE (VarE "and") [a, b]) = do
    ae <- mkExpr s a
    be <- mkExpr s b
    withvc s $ \vc -> c_vc_andExpr vc ae be
mkExpr s (VarE nm) = do
    vars <- readIORef (stp_vars s)
    case Map.lookup nm vars of
        Just v -> return v
        Nothing -> error $ "STP: unknown var: " ++ nm
mkExpr _ e = error $ "TODO: STP.mkExpr " ++ show e

instance Solver STP where
    pretty _ = C.pretty

    initialize = do
        ptr <- c_vc_createValidityChecker
        fvc <- F.newForeignPtr ptr (c_vc_Destroy ptr)
        vars <- newIORef Map.empty
        return $ STP { stp_fvc = fvc, stp_vars = vars }

    run s (Declare nm t) = do
        st <- mkType s t        
        v <- withvc s $ \vc -> (withCString nm $ \cnm -> c_vc_varExpr vc cnm st)
        modifyIORef (stp_vars s) $ Map.insert nm v

    run s (Assert e) = do
        se <- mkExpr s e
        withvc s $ \vc -> c_vc_assertFormula vc se
        
    run _ cmd = error $ "TODO: STP.run " ++ show cmd
    
    -- To check for satisfiability, we query if False is valid. If False is
    -- valid, the assertions imply False, meaning they are unsatisfiable. If
    -- False is not valid, then there's some assignment which satisfies all
    -- the assertions.
    check s = do
        false <- withvc s c_vc_falseExpr
        r <- withvc s $ \vc -> c_vc_query vc false
        case r of
            0 -> return Satisfiable     -- False is INVALID
            1 -> return Unsatisfiable   -- False is VALID
            _ -> error $ "STP.check: vc_query returned " ++ show r
        
    getIntegerValue _ _ = error $ "STP does not support free Integers"

    getBoolValue s nm = do
        var <- mkExpr s (varE nm)
        val <- withvc s $ \vc -> c_vc_getCounterExample vc var
        b <- c_vc_isBool val
        case b of
            0 -> return False
            1 -> return True
            x -> error $ "STP.getBoolValue got value " ++ show x ++ " for " ++ nm
        
    getBitVectorValue _ _ = error $ "TODO: STP.getBitVectorValue"

stp :: Q.Query STP a -> Q.Query STP a
stp = id

