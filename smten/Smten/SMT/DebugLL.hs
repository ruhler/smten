
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.SMT.DebugLL (debugll) where

import Data.IORef
import System.IO

import qualified Smten.HashTable as HT
import qualified Smten.SMT.AST as AST
import qualified Smten.SMT.Assert as A
import Smten.SMT.Solver
import Smten.Name
import Smten.Sig
import Smten.Ppr

data DebugLL = DebugLL {
    dbg_handle :: Handle,
    dbg_solver :: Solver,
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
dbgOp op dbg a b = dbgNew dbg $ a ++ " " ++ op ++ " " ++ b

instance AST.AST DebugLL String where
    fresh dbg = fresh (dbg_solver dbg) 
    assert dbg e = dbgPutStrLn dbg $ "assert " ++ e
    literal dbg l = dbgNew dbg $ pretty l
    bool dbg b = dbgNew dbg $ show b
    var dbg n = return (unname n)
    ite dbg p a b = dbgNew dbg $ p ++ " ? " ++ a ++ " : " ++ b
    unary = HT.table [(name "Smten.Bit.__prim_not_Bit", \dbg x -> dbgNew dbg $ "~" ++ x)]
    binary = HT.table [
        (name "Prelude.__prim_eq_Integer", dbgOp "=="),
        (name "Prelude.__prim_add_Integer", dbgOp "+"),
        (name "Prelude.__prim_sub_Integer", dbgOp "-"),
        (name "Prelude.__prim_mul_Integer", dbgOp "*"),
        (name "Prelude.__prim_lt_Integer", dbgOp "<"),
        (name "Prelude.__prim_leq_Integer", dbgOp "<="),
        (name "Prelude.__prim_gt_Integer", dbgOp ">"),
        (name "Prelude.__prim_geq_Integer", dbgOp ">="),
        (name "Smten.Bit.__prim_eq_Bit", dbgOp "=="),
        (name "Smten.Bit.__prim_lt_Bit", dbgOp "<"),
        (name "Smten.Bit.__prim_leq_Bit", dbgOp "<="),
        (name "Smten.Bit.__prim_gt_Bit", dbgOp ">"),
        (name "Smten.Bit.__prim_geq_Bit", dbgOp ">="),
        (name "Smten.Bit.__prim_add_Bit", dbgOp "+"),
        (name "Smten.Bit.__prim_sub_Bit", dbgOp "-"),
        (name "Smten.Bit.__prim_mul_Bit", dbgOp "*"),
        (name "Smten.Bit.__prim_or_Bit", dbgOp "|"),
        (name "Smten.Bit.__prim_and_Bit", dbgOp "&"),
        (name "Smten.Bit.__prim_concat_Bit", dbgOp "++"),
        (name "Smten.Bit.__prim_shl_Bit", dbgOp "<<"),
        (name "Smten.Bit.__prim_lshr_Bit", dbgOp ">>")
        ]
    zeroextend dbg a l = dbgNew dbg $ "zeroExtend " ++ a ++ " " ++ show l
    signextend dbg a l = dbgNew dbg $ "signExtend " ++ a ++ " " ++ show l
    extract dbg a hi lo = dbgNew dbg $ a ++ "[" ++ show hi ++ ":" ++ show lo ++ "]"
    

debugll :: FilePath -> Solver -> IO Solver
debugll f s = do
    fout <- openFile f WriteMode
    hSetBuffering fout NoBuffering
    id <- newIORef 0
    return $ Solver {
        push = hPutStrLn fout "push" >> push s,
        pop = hPutStrLn fout "pop" >> pop s,

        fresh = \t -> do
            hPutStr fout $ "fresh... "
            n <- fresh s t
            hPutStrLn fout $ pretty (Sig n t)
            return n,

        assert = \e -> do
            A.assert (DebugLL fout s id) e
            assert s e,

        check = do
            hPutStr fout $ "check... "
            r <- check s
            hPutStrLn fout $ show r
            return r,

        getIntegerValue = \n -> do
            hPutStr fout $ unname n ++ " = "
            r <- getIntegerValue s n
            hPutStrLn fout $ show r
            return r,

        getBoolValue = \n -> do
            hPutStr fout $ unname n ++ " = "
            r <- getBoolValue s n
            hPutStrLn fout $ show r
            return r,

        getBitVectorValue = \w n -> do
            hPutStr fout $ unname n ++ " = "
            r <- getBitVectorValue s w n
            hPutStrLn fout $ show r
            return r
    }

