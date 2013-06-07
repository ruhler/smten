
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.SMT.DebugLL (debugll) where

import Data.IORef
import System.IO

import Smten.Bit
import qualified Smten.SMT.AST as AST
import qualified Smten.SMT.Assert as A
import Smten.SMT.Solver

data DebugLL = DebugLL {
    dbg_handle :: Handle,
    dbg_id :: IORef Integer
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

instance AST.AST DebugLL String where
    assert dbg e = dbgPutStrLn dbg $ "assert " ++ e
    bool dbg b = dbgNew dbg $ show b
    integer dbg i = dbgNew dbg $ show i
    bit dbg w v = dbgNew dbg $ show (bv_make w v)
    var dbg n = return n
    ite dbg p a b = dbgNew dbg $ p ++ " ? " ++ a ++ " : " ++ b

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

debugll :: FilePath -> Solver -> IO Solver
debugll f s = do
    fout <- openFile f WriteMode
    hSetBuffering fout NoBuffering
    id <- newIORef 0
    return $ Solver {
        assert = \e -> do
            A.assert (DebugLL fout id) e
            assert s e,
    
        declare_bool = \nm -> do
            hPutStrLn fout $ "declare_bool " ++ nm
            declare_bool s nm,

        declare_integer = \nm -> do
            hPutStrLn fout $ "declare_integer " ++ nm
            declare_integer s nm,

        declare_bit = \nm w -> do
            hPutStrLn fout $ "declare_bit " ++ nm ++ " of width " ++ show w
            declare_bit s nm w,

        getBoolValue = \n -> do
            hPutStr fout $ n ++ " = "
            r <- getBoolValue s n
            hPutStrLn fout $ show r
            return r,

        getIntegerValue = \n -> do
            hPutStr fout $ n ++ " = "
            r <- getIntegerValue s n
            hPutStrLn fout $ show r
            return r,

        getBitVectorValue = \n w -> do
            hPutStr fout $ n ++ " = "
            r <- getBitVectorValue s n w
            hPutStrLn fout $ show (bv_make w r)
            return r,

        check = do
            hPutStr fout $ "check... "
            r <- check s
            hPutStrLn fout $ show r
            return r
    }

