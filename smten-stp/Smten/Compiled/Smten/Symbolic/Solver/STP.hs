
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Compiled.Smten.Symbolic.Solver.STP (stp) where

import qualified Data.HashTable.IO as H

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Data.Functor((<$>))

import Smten.Runtime.STPFFI
import qualified Smten.Runtime.Types as S
import Smten.Runtime.Result
import Smten.Runtime.SolverAST
import Smten.Runtime.Solver

data Formula = STPF { expr :: Ptr STP_Expr }
             | IntegerF { ints :: [(Formula, Integer)] }

type VarMap = H.BasicHashTable String (Ptr STP_Expr)

data STP = STP {
    stp_ctx :: Ptr STP_VC,
    stp_vars :: VarMap
}

stp :: Solver
stp = do
  ptr <- c_vc_createValidityChecker
  vars <- H.new
  let s = STP { stp_ctx = ptr, stp_vars = vars }
  return $ solverInstFromAST s

withvc :: STP -> (Ptr STP_VC -> IO a) -> IO a
withvc s f = f (stp_ctx s)


nointegers :: a
nointegers = error $ "STP does not support integers"

instance SolverAST STP Formula where
  declare s S.BoolT nm = do
    st <- withvc s c_vc_boolType
    v <- withvc s $ \vc -> (withCString nm $ \cnm -> c_vc_varExpr vc cnm st)
    H.insert (stp_vars s) nm v

  declare s S.IntegerT nm = nointegers

  declare s (S.BitT w) nm = do
    st <- withvc s $ \vc -> c_vc_bvType vc (fromInteger w)
    v <- withvc s $ \vc -> (withCString nm $ \cnm -> c_vc_varExpr vc cnm st)
    H.insert (stp_vars s) nm v
  
  getBoolValue s nm = do
    v <- var s nm
    val <- withvc s $ \vc -> c_vc_getCounterExample vc (expr v)
    b <- c_vc_isBool val
    case b of
        0 -> return False
        1 -> return True
        x -> error $ "STP.getBoolValue got value " ++ show x ++ " for " ++ nm

  getIntegerValue = nointegers

  getBitVectorValue s w nm = do
    v <- var s nm
    val <- withvc s $ \vc -> c_vc_getCounterExample vc (expr v)
    fromIntegral <$> c_getBVUnsignedLongLong val

  -- To check for satisfiability, we query if False is valid. If False is
  -- valid, the assertions imply False, meaning they are unsatisfiable. If
  -- False is not valid, then there's some assignment which satisfies all
  -- the assertions.
  check s = do
    false <- withvc s c_vc_falseExpr
    r <- withvc s $ \vc -> c_vc_query vc false
    case r of
        0 -> return Sat     -- False is INVALID
        1 -> return Unsat   -- False is VALID
        _ -> error $ "STP.check: vc_query returned " ++ show r
        
  assert s e = withvc s $ \vc -> c_vc_assertFormula vc (expr e)

  bool s True = STPF <$> withvc s c_vc_trueExpr
  bool s False = STPF <$> withvc s c_vc_falseExpr

  integer s i = do
    tt <- bool s True
    return (IntegerF [(tt, i)])

  bit s w v = withvc s $ \vc -> do
     let w' = fromInteger w
         v' = fromInteger v
     if w <= 64
        then STPF <$> c_vc_bvConstExprFromLL vc w' v'
        else STPF <$> (withCString (show v) $ \str ->
                         c_vc_bvConstExprFromDecStr vc w' str)

  var s nm = do
    vars <- H.lookup (stp_vars s) nm
    case vars of
        Just v -> return (STPF v)
        Nothing -> error $ "STP: unknown var: " ++ nm

  and_bool = bprim c_vc_andExpr
  not_bool = uprim c_vc_notExpr
    
  ite_bool s p a b = withvc s $ \vc -> STPF <$> c_vc_iteExpr vc (expr p) (expr a) (expr b)
  ite_bit s p a b = withvc s $ \vc -> STPF <$> c_vc_iteExpr vc (expr p) (expr a) (expr b)

  ite_integer s p a b = withvc s $ \vc -> do
    let join :: Ptr STP_Expr -> (Formula, Integer) -> IO (Formula, Integer)
        join p (a, v) = do
          pa <- STPF <$> c_vc_andExpr vc p (expr a)
          return (pa, v)
    not_p <- c_vc_notExpr vc (expr p)
    a' <- mapM (join (expr p)) (ints a)
    b' <- mapM (join not_p) (ints b)
    return $ IntegerF (a' ++ b')

  eq_integer = ibprim (==)
  leq_integer = ibprim (<=)
  add_integer = iiprim (+)
  sub_integer = iiprim (-)

  eq_bit = bprim c_vc_eqExpr
  leq_bit = bprim c_vc_bvLeExpr
  add_bit = blprim c_vc_bvPlusExpr
  sub_bit = blprim c_vc_bvMinusExpr
  mul_bit = error "TODO: STP mul_bit"
  or_bit = bprim c_vc_bvOrExpr
  and_bit = bprim c_vc_bvAndExpr
  concat_bit = bprim c_vc_bvConcatExpr
  shl_bit s _ = blprim c_vc_bvLeftShiftExprExpr s
  lshr_bit = blprim c_vc_bvRightShiftExprExpr
  not_bit = uprim c_vc_bvNotExpr
  sign_extend_bit s fr to x = withvc s $ \vc ->
    STPF <$> c_vc_bvSignExtend vc (expr x) (fromInteger to)
  extract_bit s hi lo x = withvc s $ \vc ->
    STPF <$> c_vc_bvExtract vc (expr x) (fromInteger hi) (fromInteger lo)

uprim :: (Ptr STP_VC -> Ptr STP_Expr -> IO (Ptr STP_Expr))
      -> STP -> Formula -> IO Formula
uprim f s a = withvc s $ \vc -> STPF <$> f vc (expr a)

bprim :: (Ptr STP_VC -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr))
      -> STP -> Formula -> Formula -> IO Formula
bprim f s a b = withvc s $ \vc -> STPF <$> f vc (expr a) (expr b)

blprim :: (Ptr STP_VC -> CInt -> Ptr STP_Expr -> Ptr STP_Expr -> IO (Ptr STP_Expr))
       -> STP -> Formula -> Formula -> IO Formula
blprim f s a b = withvc s $ \vc -> STPF <$> do
    n <- c_vc_getBVLength vc (expr a)
    f vc n (expr a) (expr b)

ibprim :: (Integer -> Integer -> Bool) 
       -> STP -> Formula -> Formula -> IO Formula
ibprim f s a b = withvc s $ \vc -> do
  let join :: (Formula, Integer) -> (Formula, Integer) -> IO Formula
      join (pa, va) (pb, vb) = do
        pab <- c_vc_andExpr vc (expr pa) (expr pb)
        v <- bool s (f va vb)
        STPF <$> c_vc_andExpr vc pab (expr v)

      orN :: [Formula] -> IO Formula
      orN [] = bool s False
      orN [x] = return x
      orN (x:xs) = do
        xs' <- orN xs
        STPF <$> c_vc_orExpr vc (expr x) (expr xs')
  vals <- sequence [join ax bx | ax <- ints a, bx <- ints b]
  orN vals

iiprim :: (Integer -> Integer -> Integer) 
       -> STP -> Formula -> Formula -> IO Formula
iiprim f s a b = withvc s $ \vc -> do
  let join :: (Formula, Integer) -> (Formula, Integer) -> IO (Formula, Integer)
      join (pa, va) (pb, vb) = do
        pab <- STPF <$> c_vc_andExpr vc (expr pa) (expr pb)
        let vab = f va vb
        return (pab, vab)
  IntegerF <$> sequence [join ax bx | ax <- ints a, bx <- ints b]

