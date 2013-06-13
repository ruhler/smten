
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.SMT.DebugLL (debugll) where

import Data.IORef
import System.IO

import Smten.Bit
import Smten.SMT.Solver.Static
import qualified Smten.SMT.Solver.Dynamic as D

data DebugLL = DebugLL {
    dbg_handle :: Handle,
    dbg_id :: IORef Integer,
    dbg_s :: D.Solver
}

dbgPutStrLn :: DebugLL -> String -> IO ()
dbgPutStrLn dbg s = hPutStrLn (dbg_handle dbg) s

dbgNew :: DebugLL -> String -> IO String
dbgNew dbg s = do
    nid <- readIORef (dbg_id dbg)
    modifyIORef' (dbg_id dbg) (+ 1)
    let nm = "$" ++ show nid
    dbgPutStrLn dbg $ nm ++ " = " ++ s
    return nm

dbgOp :: String -> DebugLL -> String -> String -> IO String
dbgOp op dbg a b = dbgNew dbg $ a ++ op ++ b

instance Solver DebugLL String where
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
        dbgPutStrLn dbg $ n ++ " = "
        r <- D.getBoolValue (dbg_s dbg) n
        dbgPutStrLn dbg $ show r
        return r

    getIntegerValue dbg n = do
        dbgPutStrLn dbg $ n ++ " = "
        r <- D.getIntegerValue (dbg_s dbg) n
        dbgPutStrLn dbg $ show r
        return r

    getBitVectorValue dbg n w = do
        dbgPutStrLn dbg $ n ++ " = "
        r <- D.getBitVectorValue (dbg_s dbg) n w
        dbgPutStrLn dbg $ show (bv_make w r)
        return r

    check dbg = do
        dbgPutStrLn dbg $ "check... "
        r <- D.check (dbg_s dbg)
        dbgPutStrLn dbg $ show r
        return r

    -- Note: this is overridden when we create the dynamic solver to call the
    -- underlying solver's assert method.
    assert dbg e = dbgPutStrLn dbg $ "assert " ++ e

    bool dbg b = dbgNew dbg $ show b
    integer dbg i = dbgNew dbg $ show i
    bit dbg w v = dbgNew dbg $ show (bv_make w v)
    var dbg n = return n

    ite_bool dbg p a b = dbgNew dbg $ p ++ " ? " ++ a ++ " : " ++ b
    ite_integer dbg p a b = dbgNew dbg $ p ++ " ? " ++ a ++ " : " ++ b
    ite_bit dbg p a b = dbgNew dbg $ p ++ " ? " ++ a ++ " : " ++ b

    eq_integer = dbgOp "=="
    leq_integer = dbgOp "<="
    add_integer = dbgOp "+"
    sub_integer = dbgOp "-"

    eq_bit = dbgOp "=="
    leq_bit = dbgOp "<="
    add_bit = dbgOp "+"
    sub_bit = dbgOp "-"
    mul_bit = dbgOp "*"
    or_bit = dbgOp "|"
    and_bit = dbgOp "&"

debugll :: FilePath -> D.Solver -> IO D.Solver
debugll f s = do
    fout <- openFile f WriteMode
    hSetBuffering fout NoBuffering
    id <- newIORef 0
    let dbgs = D.dynsolver (DebugLL fout id s)
    return $ dbgs {
        D.assert = \e -> D.assert dbgs e >> D.assert s e
    }

