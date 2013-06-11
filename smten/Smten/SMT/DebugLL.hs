
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.SMT.DebugLL (debugll) where

import Data.IORef
import System.IO

import Data.Dynamic

import Smten.Bit
import qualified Smten.SMT.AST as AST
import qualified Smten.SMT.Assert as A
import Smten.SMT.Solver

box :: String -> Dynamic
box = toDyn

unbox :: Dynamic -> String
unbox = flip fromDyn (error "DebugLL.unbox")

data DebugLL = DebugLL {
    dbg_handle :: Handle,
    dbg_id :: IORef Integer
}

dbgPutStrLn :: DebugLL -> String -> IO ()
dbgPutStrLn dbg s = hPutStrLn (dbg_handle dbg) s

dbgNew :: DebugLL -> String -> IO Dynamic
dbgNew dbg s = do
    nid <- readIORef (dbg_id dbg)
    modifyIORef' (dbg_id dbg) (+ 1)
    let nm = "$" ++ show nid
    dbgPutStrLn dbg $ nm ++ " = " ++ s
    return (box nm)

dbgOp :: String -> DebugLL -> Dynamic -> Dynamic -> IO Dynamic
dbgOp op dbg a b = dbgNew dbg $ unbox a ++ op ++ unbox b

ite :: DebugLL -> Dynamic -> Dynamic -> Dynamic -> IO Dynamic
ite dbg p a b = dbgNew dbg $ unbox p ++ " ? " ++ unbox a ++ " : " ++ unbox b

instance AST.AST DebugLL where
    assert dbg e = dbgPutStrLn dbg $ "assert " ++ unbox e
    bool dbg b = dbgNew dbg $ show b
    integer dbg i = dbgNew dbg $ show i
    bit dbg w v = dbgNew dbg $ show (bv_make w v)
    var dbg n = return (box n)

    ite_bool = ite
    ite_integer = ite
    ite_bit = ite

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

