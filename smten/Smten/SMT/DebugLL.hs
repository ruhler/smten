
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.SMT.DebugLL (debugll) where

import System.IO

import Smten.Bit
import Smten.SMT.Solver.Static
import Smten.Runtime.Debug
import qualified Smten.SMT.Solver.Dynamic as D

data DebugLL = DebugLL {
    dbg_handle :: Handle,
    dbg_s :: D.Solver
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

instance Solver DebugLL Debug where
    declare_bool dbg nm = do
        dbgPutStrLn dbg $ "delare_bool " ++ nm
        D.declare_bool (dbg_s dbg) nm

    declare_integer dbg nm = do
        dbgPutStrLn dbg $ "delare_integer " ++ nm
        D.declare_integer (dbg_s dbg) nm

    declare_bit dbg nm w = do
        dbgPutStrLn dbg $ "delare_bit " ++ nm ++ " " ++ show w
        D.declare_bit (dbg_s dbg) nm w

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

    getBitVectorValue dbg n w = do
        dbgPutStr dbg $ n ++ " = "
        r <- D.getBitVectorValue (dbg_s dbg) n w
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
    sign_extend_bit dbg n x = return $ dbgText "?SignExtend"
    extract_bit dbg hi lo x = return $
      dbgApps (dbgText "extract") [dbgText $ "[" ++ show hi ++ ":" ++ show lo ++ "]", sh x]

debugll :: FilePath -> D.Solver -> IO D.Solver
debugll f s = do
    fout <- openFile f WriteMode
    hSetBuffering fout NoBuffering
    let dbgs = D.dynsolver (DebugLL fout s)
    return $ dbgs {
        D.assert = \e -> ({-# SCC "DebugLL" #-} D.assert dbgs e) >> D.assert s e
    }

