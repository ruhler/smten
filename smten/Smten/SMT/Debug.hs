
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.SMT.Debug (debug) where

import System.IO

import Smten.Bit
import Smten.SMT.Solver.Dynamic
import Smten.Runtime.Debug (dbgRender)
import qualified Smten.Runtime.SmtenHS as S

debug :: FilePath -> Solver -> IO Solver
debug f s = do
    fout <- openFile f WriteMode
    hSetBuffering fout NoBuffering
    return $ Solver {
        assert = \e -> do
            hPutStrLn fout $ dbgRender (S.debug e)
            assert s e,

        declare_bool = \nm -> do
            hPutStrLn fout $ "delare_bool " ++ nm
            declare_bool s nm,

        declare_integer = \nm -> do
            hPutStrLn fout $ "delare_integer " ++ nm
            declare_integer s nm,

        declare_bit = \nm w -> do
            hPutStrLn fout $ "delare_bit " ++ nm ++ " " ++ show w
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
            hPutStrLn fout $ "check... "
            r <- check s
            hPutStrLn fout $ show r
            return r
    }

