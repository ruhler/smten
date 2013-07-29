
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Compiled.Smten.Symbolic.Solver.Debug (debug) where

import System.IO

import Smten.Runtime.Bit
import Smten.Runtime.Debug
import Smten.Runtime.SolverAST
import Smten.Runtime.Solver as D
import qualified Smten.Compiled.Smten.Smten.Base as S

data DebugLL = DebugLL {
    dbg_handle :: Handle,
    dbg_s :: SolverInst
}

dbgPutStr :: DebugLL -> String -> IO ()
dbgPutStr dbg s = hPutStr (dbg_handle dbg) s

dbgPutStrLn :: DebugLL -> String -> IO ()
dbgPutStrLn dbg s = hPutStrLn (dbg_handle dbg) s

-- mark a debug object for sharing.
sh :: Debug -> Debug
sh x = dbgShare id x

op :: String -> DebugLL -> Debug -> Debug -> IO Debug
op o _ a b = return $ dbgOp o (sh a) (sh b)

instance SolverAST DebugLL Debug where
    declare dbg ty nm = do
        dbgPutStrLn dbg $ "delare " ++ nm ++ " :: " ++ show ty
        D.declare (dbg_s dbg) ty nm

    getBoolValue dbg n = do
        dbgPutStr dbg $ n ++ " = "
        r <- D.getBoolValue (dbg_s dbg) n
        dbgPutStrLn dbg $ show r
        return r

    getIntegerValue dbg n = do
        dbgPutStr dbg $ n ++ " = "
        r <- D.getIntegerValue (dbg_s dbg) n
        dbgPutStrLn dbg $ show r
        return r

    getBitVectorValue dbg w n = do
        dbgPutStr dbg $ n ++ " = "
        r <- D.getBitVectorValue (dbg_s dbg) w n
        dbgPutStrLn dbg $ show (bv_make w r)
        return r

    check dbg = do
        dbgPutStrLn dbg $ "check... "
        r <- D.check (dbg_s dbg)
        dbgPutStrLn dbg $ show r
        return r

    assert dbg e = do
        dbgPutStrLn dbg "assert:"
        dbgstr <- dbgRender e
        dbgPutStrLn dbg $ dbgstr

    bool dbg b = return $ dbgLit b
    integer dbg i = return $ dbgLit i
    bit dbg w v = return $ dbgLit (bv_make w v)
    var dbg n = return $ dbgVar n

    and_bool = op "&&"
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
    shl_bit = op "<<"
    lshr_bit = op ">>"
    not_bit dbg x = return $ dbgApp (dbgText "~") (sh x)
--    sign_extend_bit dbg n x = return $ dbgText "?SignExtend"
--    extract_bit dbg hi lo x = return $
--      dbgApps (dbgText "extract") [dbgText $ "[" ++ show hi ++ ":" ++ show lo ++ "]", sh x]

debug :: S.List__ S.Char -> Solver -> Solver
debug fsmten s = do
    let f = S.toHSString fsmten
    fout <- openFile f WriteMode
    hSetBuffering fout NoBuffering
    slv <- s
    let dbgs = solverInstFromAST (DebugLL fout slv)
    return $ dbgs {
        D.assert = \e -> ({-# SCC "DebugAssert" #-} D.assert dbgs e) >> D.assert slv e
    }

