
-- | Run a Seri Exp of type IO in the haskell IO monad.
module Seri.IO.Run (run) where

import Seri.Lambda
import Seri.Elaborate

-- | Given a Seri expression of type IO a,
-- returns the Seri expression of type a which results from running the IO
-- computation.
run :: Env -> Exp -> IO Exp
run env e = do
    case elabwhnf env e of
        (AppE (VarE (Sig n _)) [arg]) | n == name "Seri.IO.IO.putStr" -> do
            case deStringE (elabwhnf env arg) of
                Just str -> putStr str
                Nothing -> error $ "putStr: expected string, got: " ++ pretty arg
            return unitE
--        (AppE (VarE (Sig n _)) [arg]) | n == name "Seri.IO.IO.runQuery" -> do
--            runQuery (RunOptions Nothing True) env (Seri.SMT.run arg)
        (AppE (VarE (Sig n _)) [x]) | n == name "Seri.IO.IO.return_io" -> return x
        (AppE (VarE (Sig n _)) [x, f]) | n == name "Seri.IO.IO.bind_io" -> do
          result <- run env x
          run env (AppE f [result])
        (AppE (VarE (Sig n _)) [x, y]) | n == name "Seri.IO.IO.nobind_io" -> do
          run env x
          run env y
        x -> error $ "unknown IO: " ++ pretty x

