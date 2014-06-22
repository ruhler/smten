
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Compiled.Smten.Search.Solver.Debug (debug) where

import System.IO

import Smten.Runtime.Debug
import Smten.Runtime.Debug.Finite
import Smten.Runtime.FreeID
import Smten.Runtime.Model
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

debug :: S.List__ S.Char -> Solver -> IO Solver
debug fsmten s = do
  let f = S.toHSString fsmten
  fout <- openFile f WriteMode
  hSetBuffering fout NoBuffering
  let dbg = DebugLL fout
  return . Solver $ \formula -> do
     dbgPutStrLn dbg $ ""

     dbgPutStrLn dbg "assert:"
     dbgstr <- dbgRender (debug_BoolFF formula)
     dbgPutStrLn dbg $ dbgstr

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

