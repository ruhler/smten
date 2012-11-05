
{-# LANGUAGE PatternGuards #-}

module Seri.SMT.STP.STP (STP(), stp) where

import Data.Functor
import Data.List(genericLength)
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

mkBinExpr :: STP -> Expression -> Expression
      -> (Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr))
      -> IO (Ptr STP_Expr)
mkBinExpr s a b f = do
    ae <- mkExpr s a
    be <- mkExpr s b
    withvc s $ \vc -> f vc ae be

mkExpr :: STP -> Expression -> IO (Ptr STP_Expr)
mkExpr s (LitE (BoolL True)) = withvc s c_vc_trueExpr
mkExpr s (LitE (BoolL False)) = withvc s c_vc_falseExpr
mkExpr s (LitE (IntegerL {})) = error $ "STP does not support integer literals"
mkExpr s e | Just (a, b) <- de_eqE e = mkBinExpr s a b c_vc_eqExpr
mkExpr s e | Just [a, b] <- de_orE e = mkBinExpr s a b c_vc_orExpr
mkExpr s e | Just [a, b] <- de_andE e = mkBinExpr s a b c_vc_andExpr
mkExpr s e | Just (a, b) <- de_bvorE e = mkBinExpr s a b c_vc_bvOrExpr
mkExpr s e | Just (a, b) <- de_bvandE e = mkBinExpr s a b c_vc_bvAndExpr
mkExpr s e | Just (a, b) <- de_bvconcatE e = mkBinExpr s a b c_vc_bvConcatExpr
mkExpr s e | Just a <- de_bvnotE e = do
    ae <- mkExpr s a
    withvc s $ \vc -> c_vc_bvNotExpr vc ae
mkExpr s e | Just (a, n) <- de_bvshiftLeft0E e = do
    ae <- mkExpr s a
    withvc s $ \vc -> c_vc_bvLeftShiftExpr vc (fromInteger n) ae
mkExpr s e | Just (a, n) <- de_bvzeroExtendE e = do
    ae <- mkExpr s a
    zeros <- withvc s $ \vc -> c_vc_bvConstExprFromLL vc (fromInteger n) (fromInteger 0)
    withvc s $ \vc -> c_vc_bvConcatExpr vc zeros ae
mkExpr s e | Just args <- de_orE e = do
    args' <- mapM (mkExpr s) args
    withvc s $ \vc ->
        withArray args' $ \ptr -> c_vc_orExprN vc ptr (genericLength args)
mkExpr s e | Just args <- de_andE e = do
    args' <- mapM (mkExpr s) args
    withvc s $ \vc ->
        withArray args' $ \ptr -> c_vc_andExprN vc ptr (genericLength args)
mkExpr s e | Just a <- de_notE e = do
    ae <- mkExpr s a
    withvc s $ \vc -> c_vc_notExpr vc ae
mkExpr s e | Just (p, a, b) <- de_ifE e = do
    [pe, ae, be] <- mapM (mkExpr s) [p, a, b]
    withvc s $ \vc -> c_vc_iteExpr vc pe ae be
mkExpr s e | Just (a, b) <- de_bvaddE e = do
    ae <- mkExpr s a
    be <- mkExpr s b
    w <- withvc s $ \vc -> c_vc_getBVLength vc ae
    withvc s $ \vc -> c_vc_bvPlusExpr vc w ae be
mkExpr s e | Just (a, b) <- de_bvsubE e = do
    ae <- mkExpr s a
    be <- mkExpr s b
    w <- withvc s $ \vc -> c_vc_getBVLength vc ae
    withvc s $ \vc -> c_vc_bvMinusExpr vc w ae be
mkExpr s e | Just (w, v) <- de_mkbvE e = do
    withvc s $ \vc -> c_vc_bvConstExprFromLL vc (fromInteger w) (fromInteger v)
mkExpr s (VarE nm) = do
    vars <- readIORef (stp_vars s)
    case Map.lookup nm vars of
        Just v -> return v
        Nothing -> error $ "STP: unknown var: " ++ nm
mkExpr s e | Just (bs, v) <- de_letE e =
  let mkvar :: (String, Expression) -> IO ()
      mkvar (nm, val) = do
        val' <- mkExpr s val
        modifyIORef (stp_vars s) $ Map.insert nm val'
  in do
    mapM_ mkvar bs
    mkExpr s v
    
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

    run s Push = withvc s c_vc_push
    run s Pop = withvc s c_vc_pop
        
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
        
    getBitVectorValue s _ nm = do
        var <- mkExpr s (varE nm)
        val <- withvc s $ \vc -> c_vc_getCounterExample vc var
        fromIntegral <$> c_getBVUnsignedLongLong val
        

stp :: Q.Query STP a -> Q.Query STP a
stp = id

