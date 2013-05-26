
module Smten.SMT.Debug (debug) where

import System.IO

import Smten.SMT.Solver
import Smten.Name
import Smten.Sig
import Smten.Ppr

debug :: FilePath -> Solver -> IO Solver
debug f s = do
    fout <- openFile f WriteMode
    hSetBuffering fout NoBuffering
    return $ Solver {
        push = hPutStrLn fout "push" >> push s,
        pop = hPutStrLn fout "pop" >> pop s,

        fresh = \t -> do
            hPutStr fout $ "fresh... "
            n <- fresh s t
            hPutStrLn fout $ pretty (Sig n t)
            return n,

        assert = \e -> do
            hPutStrLn fout $ "assert " ++ pretty e
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

