
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -auto-all #-}

module Smten.Compiled.Smten.Symbolic.Solver.STP (stp) where

import qualified Data.HashTable.IO as H

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Data.Functor((<$>))
import Data.IORef

import Smten.Runtime.Build
import Smten.Runtime.STPFFI
import Smten.Runtime.FreeID
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula.Type
import Smten.Runtime.Result
import Smten.Runtime.SolverAST
import Smten.Runtime.Solver
import Smten.Runtime.Integers

type VarMap = H.BasicHashTable FreeID STP_Expr

data STP = STP {
    stp_ctx :: STP_VC,
    stp_vars :: VarMap,

    -- Keep a list of all the expressions made so we can garbage collect them
    -- when we are done.
    stp_gc :: IORef [STP_Expr]
}

-- | Add the given expression to the gc list.
gce :: STP -> STP_Expr -> IO ()
gce s x = modifyIORef (stp_gc s) ((:) x)

gceM :: STP -> IO STP_Expr -> IO STP_Expr
gceM s v = do
    x <- v
    gce s x
    return x

type STP_WITH_I = Integers STP STP_Expr STP_Expr STP_Expr

{-# SPECIALIZE build :: STP_WITH_I -> BoolFF -> IO (STP_Expr, [(FreeID, Type)]) #-}
{-# SPECIALIZE solverFromAST :: IO STP_WITH_I -> Solver #-}

stp :: Solver
stp = solverFromAST $ do
  ptr <- c_vc_createValidityChecker
  vars <- H.new
  gc <- newIORef []
  let s = STP { stp_ctx = ptr, stp_vars = vars, stp_gc = gc }
  addIntegers s :: IO STP_WITH_I

withvc :: STP -> (STP_VC -> IO a) -> IO a
withvc s f = f (stp_ctx s)

nointegers :: a
nointegers = error $ "STP does not support integers"

getbits :: STP -> Integer -> STP_Expr -> IO Integer
getbits s w e
  | w <= 64 = fromIntegral <$> c_getBVUnsignedLongLong e
  | otherwise = do
      elo <- gceM s (withvc s $ \vc -> c_vc_bvExtract vc e 63 0)
      lo <- getbits s 64 elo

      ehi <- gceM s (withvc s $ \vc -> c_vc_bvRightShiftExpr vc 64 e)
      hi <- getbits s (w-64) ehi
      return $ shiftL hi 64 + lo

instance SolverAST STP STP_Expr STP_Expr STP_Expr where
  declare_bool s nm = do
    -- Don't gc the result of c_vc_boolType, because stp does it for us.
    st <- withvc s c_vc_boolType
    v <- withvc s $ \vc ->
           withCString (freenm nm) $ \cnm ->
             gceM s $ c_vc_varExpr vc cnm st
    H.insert (stp_vars s) nm v

  declare_integer s nm = nointegers

  declare_bit s w nm = do
    st <- withvc s $ \vc -> 
            -- No need to gc the result of c_vc_bvType, because stp does it
            -- for us.
            c_vc_bvType vc (fromInteger w)

    v <- withvc s $ \vc -> 
           withCString (freenm nm) $ \cnm ->
              gceM s $ c_vc_varExpr vc cnm st
    H.insert (stp_vars s) nm v
  
  getBoolValue s nm = do
    v <- var_bool s nm
    val <- withvc s $ \vc ->
             gceM s $ c_vc_getCounterExample vc v
    b <- c_vc_isBool val
    case b of
        0 -> return False
        1 -> return True
        x -> error $ "STP.getBoolValue got value " ++ show x ++ " for " ++ freenm nm

  getIntegerValue = nointegers

  getBitVectorValue s w nm = do
    v <- var_bit s w nm
    withvc s $ \vc -> do
      val <- gceM s $ c_vc_getCounterExample vc v
      getbits s w val

  -- To check for satisfiability, we query if False is valid. If False is
  -- valid, the assertions imply False, meaning they are unsatisfiable. If
  -- False is not valid, then there's some assignment which satisfies all
  -- the assertions.
  check s = {-# SCC "STPCheck" #-} do
    false <- gceM s $ withvc s c_vc_falseExpr
    r <- withvc s $ \vc -> c_vc_query vc false
    case r of
        0 -> return Sat     -- False is INVALID
        1 -> return Unsat   -- False is VALID
        _ -> error $ "STP.check: vc_query returned " ++ show r

  cleanup s = do
     exprs <- readIORef (stp_gc s)
     mapM c_vc_DeleteExpr exprs
     withvc s c_vc_Destroy
        
  assert s e = {-# SCC "STPAssert" #-} withvc s $ \vc -> do
      c_vc_assertFormula vc e

  bool s True = gceM s $ withvc s c_vc_trueExpr
  bool s False = gceM s $ withvc s c_vc_falseExpr

  integer = nointegers

  bit s w v = withvc s $ \vc -> do
     let w' = fromInteger w
         v' = fromInteger v
     if w <= 64
        then gceM s $ c_vc_bvConstExprFromLL vc w' v'
        else withCString (show v) $ \str ->
                 gceM s $ c_vc_bvConstExprFromDecStr vc w' str

  var_bool s nm = do
    vars <- H.lookup (stp_vars s) nm
    case vars of
        Just v -> return v
        Nothing -> error $ "STP: unknown var: " ++ freenm nm
  var_integer s nm = do
    vars <- H.lookup (stp_vars s) nm
    case vars of
        Just v -> return v
        Nothing -> error $ "STP: unknown var: " ++ freenm nm
  var_bit s w nm = do
    vars <- H.lookup (stp_vars s) nm
    case vars of
        Just v -> return v
        Nothing -> error $ "STP: unknown var: " ++ freenm nm

  and_bool = bprim c_vc_andExpr
  or_bool = bprim c_vc_orExpr
  not_bool = uprim c_vc_notExpr
    
  ite_bool s p a b = withvc s $ \vc -> gceM s $ c_vc_iteExpr vc p a b
  ite_bit s p a b = withvc s $ \vc -> gceM s $ c_vc_iteExpr vc p a b

  ite_integer = nointegers
  eq_integer = nointegers
  leq_integer = nointegers
  add_integer = nointegers
  sub_integer = nointegers

  eq_bit = bprim c_vc_eqExpr
  leq_bit = bprim c_vc_bvLeExpr
  add_bit = blprim c_vc_bvPlusExpr
  sub_bit = blprim c_vc_bvMinusExpr
  mul_bit = blprim c_vc_bvMultExpr
  sdiv_bit = blprim c_vc_sbvDivExpr
  srem_bit = blprim c_vc_sbvRemExpr
  smod_bit = blprim c_vc_sbvModExpr
  udiv_bit = blprim c_vc_bvDivExpr
  urem_bit = blprim c_vc_bvModExpr
  or_bit = bprim c_vc_bvOrExpr
  and_bit = bprim c_vc_bvAndExpr
  concat_bit = bprim c_vc_bvConcatExpr
  shl_bit s _ = blprim c_vc_bvLeftShiftExprExpr s
  lshr_bit s _ = blprim c_vc_bvRightShiftExprExpr s
  not_bit = uprim c_vc_bvNotExpr
  sign_extend_bit s fr to x = withvc s $ \vc ->
    gceM s $ c_vc_bvSignExtend vc x (fromInteger to)
  extract_bit s hi lo x = withvc s $ \vc ->
    gceM s $ c_vc_bvExtract vc x (fromInteger hi) (fromInteger lo)

uprim :: (STP_VC -> STP_Expr -> IO STP_Expr)
      -> STP -> STP_Expr -> IO STP_Expr
uprim f s a = withvc s $ \vc -> gceM s $ f vc a

bprim :: (STP_VC -> STP_Expr -> STP_Expr -> IO STP_Expr)
      -> STP -> STP_Expr -> STP_Expr -> IO STP_Expr
bprim f s a b = withvc s $ \vc -> gceM s $ f vc a b

blprim :: (STP_VC -> CInt -> STP_Expr -> STP_Expr -> IO STP_Expr)
       -> STP -> STP_Expr -> STP_Expr -> IO STP_Expr
blprim f s a b = withvc s $ \vc -> do
    n <- c_vc_getBVLength vc a
    gceM s $ f vc n a b

