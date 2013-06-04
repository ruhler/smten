
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.SMT.DebugLL (debugll) where

import Data.IORef
import System.IO

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

instance AST.AST DebugLL String where
    assert dbg e = dbgPutStrLn dbg $ "assert " ++ e
    bool dbg b = dbgNew dbg $ show b
    var dbg n = return n
    ite dbg p a b = dbgNew dbg $ p ++ " ? " ++ a ++ " : " ++ b

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

        getBoolValue = \n -> do
            hPutStr fout $ n ++ " = "
            r <- getBoolValue s n
            hPutStrLn fout $ show r
            return r,

        check = do
            hPutStr fout $ "check... "
            r <- check s
            hPutStrLn fout $ show r
            return r
    }

