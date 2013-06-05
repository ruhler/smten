
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.SMT.STP.STP (stp) where

import qualified Data.HashTable.IO as H

import Foreign
import Foreign.C.String

import Smten.SMT.STP.FFI
import Smten.SMT.AST
import qualified Smten.SMT.Assert as A
import qualified Smten.SMT.Solver as S

type VarMap = H.BasicHashTable String (Ptr STP_Expr)

data STP = STP {
    stp_ctx :: Ptr STP_VC,
    stp_vars :: VarMap
}

stp :: IO S.Solver
stp = do
  ptr <- c_vc_createValidityChecker
  vars <- H.new
  let s = STP { stp_ctx = ptr, stp_vars = vars }
  return $ S.Solver {
       S.assert = A.assert s,
       S.declare_bool = stpdeclare_bool s,
       S.declare_integer = nointegers,
       S.getBoolValue = getBoolValue s,
       S.getIntegerValue = nointegers,
       S.check = check s
    }

stpdeclare_bool :: STP -> String -> IO ()
stpdeclare_bool s nm = do
    st <- withvc s c_vc_boolType
    v <- withvc s $ \vc -> (withCString nm $ \cnm -> c_vc_varExpr vc cnm st)
    H.insert (stp_vars s) nm v
        
withvc :: STP -> (Ptr STP_VC -> IO a) -> IO a
withvc s f = f (stp_ctx s)

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

nointegers :: a
nointegers = error $ "STP does not support integers"

instance AST STP (Ptr STP_Expr) where
  assert s e = withvc s $ \vc -> c_vc_assertFormula vc e
  integer = nointegers
  bool s True = withvc s c_vc_trueExpr
  bool s False = withvc s c_vc_falseExpr
  var s nm = do
    vars <- H.lookup (stp_vars s) nm
    case vars of
        Just v -> return v
        Nothing -> error $ "STP: unknown var: " ++ nm
  ite s p a b = withvc s $ \vc -> c_vc_iteExpr vc p a b
  eq_integer = nointegers
  leq_integer = nointegers
  add_integer = nointegers
  sub_integer = nointegers


getBoolValue :: STP -> String -> IO Bool
getBoolValue s nm = do
    v <- var s nm
    val <- withvc s $ \vc -> c_vc_getCounterExample vc v
    b <- c_vc_isBool val
    case b of
        0 -> return False
        1 -> return True
        x -> error $ "STP.getBoolValue got value " ++ show x ++ " for " ++ nm
