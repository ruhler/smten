
{-# LANGUAGE PatternGuards #-}

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

import qualified Smten.SMT.Solver as S
import Smten.SMT.Syntax
import Smten.SMT.Translate
import qualified Smten.SMT.STP.Concrete as C
import Smten.ExpH

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
mkExpr s e | Just (a, b) <- de_bvltE e = mkBinExpr s a b c_vc_bvLtExpr
mkExpr s e | Just (a, b) <- de_bvgtE e = mkBinExpr s a b c_vc_bvGtExpr
mkExpr s e | Just (a, b) <- de_bvleqE e = mkBinExpr s a b c_vc_bvLeExpr
mkExpr s e | Just (a, b) <- de_bvgeqE e = mkBinExpr s a b c_vc_bvGeExpr
mkExpr s e | Just [a, b] <- de_orE e = mkBinExpr s a b c_vc_orExpr
mkExpr s e | Just [a, b] <- de_andE e = mkBinExpr s a b c_vc_andExpr
mkExpr s e | Just (a, b) <- de_bvorE e = mkBinExpr s a b c_vc_bvOrExpr
mkExpr s e | Just (a, b) <- de_bvandE e = mkBinExpr s a b c_vc_bvAndExpr
mkExpr s e | Just (a, b) <- de_bvconcatE e = mkBinExpr s a b c_vc_bvConcatExpr
mkExpr s e | Just a <- de_bvnotE e = do
    ae <- mkExpr s a
    withvc s $ \vc -> c_vc_bvNotExpr vc ae
mkExpr s e | Just (a, b) <- de_bvshlE e =
  let f :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)
      f vc ae be = do
         n <- c_vc_getBVLength vc ae
         c_vc_bvLeftShiftExprExpr vc n ae be
  in mkBinExpr s a b f
mkExpr s e | Just (a, b) <- de_bvlshrE e =
  let f :: Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr)
      f vc ae be = do
         n <- c_vc_getBVLength vc ae
         c_vc_bvRightShiftExprExpr vc n ae be
  in mkBinExpr s a b f
mkExpr s e | Just (a, n) <- de_bvzeroExtendE e = do
    ae <- mkExpr s a
    zeros <- withvc s $ \vc -> c_vc_bvConstExprFromLL vc (fromInteger n) (fromInteger 0)
    withvc s $ \vc -> c_vc_bvConcatExpr vc zeros ae
mkExpr s e | Just (a, n) <- de_bvsignExtendE e = do
    ae <- mkExpr s a
    withvc s $ \vc -> c_vc_bvSignExtend vc ae (fromInteger n)
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
    m <- readIORef (stp_vars s)
    mapM_ mkvar bs
    p <- mkExpr s v
    writeIORef (stp_vars s) $! m
    return p
    
mkExpr _ e = error $ "TODO: STP.mkExpr " ++ show e

stp :: IO S.Solver
stp = do
  ptr <- c_vc_createValidityChecker
  -- TODO: this leaks STP pointers?
  fvc <- F.newForeignPtr ptr (return ())
  vars <- newIORef Map.empty
  let s = STP { stp_fvc = fvc, stp_vars = vars }
  return $ S.Solver {
       S.push = push s,
       S.pop = pop s,
       S.declare = declare s,
       S.assert = assert s,
       S.check = check s,
       S.getIntegerValue = getIntegerValue s,
       S.getBoolValue = getBoolValue s,
       S.getBitVectorValue = getBitVectorValue s
    }
        
declare :: STP -> Symbol -> Type -> IO ()
declare s nm t = do
    st <- mkType s t        
    v <- withvc s $ \vc -> (withCString nm $ \cnm -> c_vc_varExpr vc cnm st)
    modifyIORef (stp_vars s) $ Map.insert nm v

assert :: STP -> ExpH -> IO ()
assert s e = do
    se <- mkExpr s $ {-# SCC "TRANSLATE" #-} smtE (fromExpH e)
    withvc s $ \vc -> c_vc_assertFormula vc se

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
    
getIntegerValue :: STP -> String -> IO Integer
getIntegerValue _ _ = error $ "STP does not support free Integers"

getBoolValue :: STP -> String -> IO Bool
getBoolValue s nm = do
    var <- mkExpr s (varE nm)
    val <- withvc s $ \vc -> c_vc_getCounterExample vc var
    b <- c_vc_isBool val
    case b of
        0 -> return False
        1 -> return True
        x -> error $ "STP.getBoolValue got value " ++ show x ++ " for " ++ nm
    
getBitVectorValue :: STP -> Integer -> String -> IO Integer
getBitVectorValue s _ nm = do
    var <- mkExpr s (varE nm)
    val <- withvc s $ \vc -> c_vc_getCounterExample vc var
    fromIntegral <$> c_getBVUnsignedLongLong val
    
