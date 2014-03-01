
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Compiled.Smten.Symbolic.Solver.Debug (debug) where

import System.IO

import Smten.Runtime.Build
import Smten.Runtime.Bit
import Smten.Runtime.Debug
import Smten.Runtime.FreeID
import Smten.Runtime.Model
import Smten.Runtime.SolverAST
import Smten.Runtime.Solver
import qualified Smten.Compiled.Smten.Smten.Base as S

data DebugLL = DebugLL {
    dbg_handle :: Handle
}

dbgPutStrLn :: DebugLL -> String -> IO ()
dbgPutStrLn dbg s = hPutStrLn (dbg_handle dbg) s

dbgModelVar :: (Show a) => DebugLL -> (FreeID, a) -> IO ()
dbgModelVar dbg (n, x) = dbgPutStrLn dbg $ freenm n ++ " = " ++ show x

dbgModel :: DebugLL -> Model -> IO ()
dbgModel dbg m = do
    mapM_ (dbgModelVar dbg) (m_bools m)
    mapM_ (dbgModelVar dbg) (m_bits m)
    mapM_ (dbgModelVar dbg) (m_integers m)

-- mark a debug object for sharing.
sh :: Debug -> Debug
sh x = dbgShare id x

op :: String -> DebugLL -> Debug -> Debug -> IO Debug
op o _ a b = return $ dbgOp o (sh a) (sh b)

instance SolverAST DebugLL Debug where
    declare_bool dbg nm = do
        dbgPutStrLn dbg $ "declare " ++ freenm nm ++ " :: Bool"
    declare_integer dbg nm = do
        dbgPutStrLn dbg $ "declare " ++ freenm nm ++ " :: Integer"
    declare_bit dbg w nm = do
        dbgPutStrLn dbg $ "declare " ++ freenm nm ++ " :: Bit " ++ show w

    getBoolValue = error $ "Debug.getBoolValue: not implemented"
    getIntegerValue = error $ "Debug.getIntegerValue: not implemented"
    getBitVectorValue = error $ "Debug.getBitVectorValue: not implemented"
    getModel = error $ "Debug.getModel: not implemented"
    check = error $ "Debug.check not implemented"

    assert dbg e = do
        dbgPutStrLn dbg "assert:"
        dbgstr <- dbgRender e
        dbgPutStrLn dbg $ dbgstr

    bool dbg b = return $ dbgLit b
    integer dbg i = return $ dbgLit i
    bit dbg w v = return $ dbgLit (bv_make w v)

    var_bool dbg n = return $ dbgVar (freenm n)
    var_integer dbg n = return $ dbgVar (freenm n)
    var_bit dbg w n = return $ dbgVar (freenm n)

    and_bool = op "&&"
    or_bool = op "||"
    not_bool dbg x = return $ dbgApp (dbgText "!") (sh x)
    ite_bool dbg p a b = return $ dbgCase "True" (sh p) (sh a) (sh b)
    ite_integer dbg p a b = return $ dbgCase "True" (sh p) (sh a) (sh b)
    ite_bit dbg p a b = return $ dbgCase "True" (sh p) (sh a) (sh b)

    eq_integer = op "=="
    leq_integer = op "<="
    add_integer = op "+"
    sub_integer = op "-"

    eq_bit = op "=="
    leq_bit = op "<="
    add_bit = op "+"
    sub_bit = op "-"
    mul_bit = op "*"
    or_bit = op "|"
    and_bit = op "&"
    concat_bit = op "++"
    shl_bit d _ = op "<<" d
    lshr_bit d _ = op ">>" d
    not_bit dbg x = return $ dbgApp (dbgText "~") (sh x)
    sign_extend_bit dbg fr to x = return $ dbgText "?SignExtend"
    extract_bit dbg hi lo x = return $
      dbgApp (sh x) (dbgText $ "[" ++ show hi ++ ":" ++ show lo ++ "]")

debug :: S.List__ S.Char -> Solver -> IO Solver
debug fsmten s = do
  let f = S.toHSString fsmten
  fout <- openFile f WriteMode
  hSetBuffering fout NoBuffering
  let dbg = DebugLL fout
  return . Solver $ \formula -> do
     dbgPutStrLn dbg $ ""
     (p, _) <- build dbg formula
     assert dbg p
     dbgPutStrLn dbg $ "check... "
     res <- solve s formula
     case res of
       Just m -> do
           dbgPutStrLn dbg "Sat"
           dbgModel dbg m
           dbgPutStrLn dbg $ ""
           return (Just m)
       Nothing -> do
           dbgPutStrLn dbg "Unsat"
           dbgPutStrLn dbg $ ""
           return Nothing

