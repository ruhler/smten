
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Tests.SMT.Test (
    SMTTest, SMTTestCfg(..),
    symtesteq, symtest, runtest
  ) where

import Smten.Prelude

import Smten.Control.Monad.Reader
import Smten.Symbolic
import Smten.Symbolic.Solver.Debug
import Smten.Tests.Test

data SMTTestCfg = SMTTestCfg {
    ss_solver :: Solver,

    -- | A list of test cases to skip.
    ss_skips :: [String],

    -- | A list of test cases to run with the debug solver
    ss_debugs :: [String]
}

type SMTTest = ReaderT SMTTestCfg IO

symtesteq :: (Eq a) => String -> Maybe a -> Symbolic a -> SMTTest ()
symtesteq nm wnt q = symtest nm ((==) wnt) q

symtest :: String -> (Maybe a -> Bool) -> Symbolic a -> SMTTest ()
symtest nm tst q = do
  cfg <- ask
  if nm `elem` ss_skips cfg 
    then liftIO . putStrLn $ nm ++ " SKIPPED"
    else do
      liftIO $ do
        putStrLn $ nm ++ "..."
        slvr <- if nm `elem` ss_debugs cfg
                      then debug (nm ++ ".dbg") (ss_solver cfg)
                      else return $ ss_solver cfg
        got <- run_symbolic slvr q
        test nm (tst got)

runtest :: SMTTestCfg -> SMTTest () -> IO ()
runtest s t = runReaderT t s

