
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Test (
    SMTTest, SMTTestCfg(..),
    symtesteq, symtest, runtest
  ) where

import Smten.Prelude

import Smten.Control.Monad.Reader
import Smten.Symbolic
import Smten.Tests.Test

data SMTTestCfg = SMTTestCfg {
    ss_solver :: Solver,
    ss_skips :: [String]
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
        got <- run_symbolic (ss_solver cfg) q
        test nm (tst got)

runtest :: SMTTestCfg -> SMTTest () -> IO ()
runtest s t = runReaderT t s

