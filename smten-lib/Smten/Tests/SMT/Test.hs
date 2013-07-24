
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Test (SMTTest, symtesteq, symtest, runtest) where

import Smten.Prelude

import Smten.Control.Monad.Reader
import Smten.Symbolic
import Smten.Tests.Test

type SMTTest = ReaderT Solver IO

symtesteq :: (Eq a) => String -> Maybe a -> Symbolic a -> SMTTest ()
symtesteq nm wnt q = symtest nm ((==) wnt) q

symtest :: String -> (Maybe a -> Bool) -> Symbolic a -> SMTTest ()
symtest nm tst q = do
  slv <- ask
  liftIO $ do
    putStrLn $ nm ++ "..."
    got <- run_symbolic slv q
    test nm (tst got)

runtest :: Solver -> SMTTest () -> IO ()
runtest s t = runReaderT t s

