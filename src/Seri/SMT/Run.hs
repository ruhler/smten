
-- | Run a Seri Exp of type Query in the haskell Query monad.
module Seri.SMT.Run (run) where

import Seri.Lambda hiding (free, query)
import Seri.SMT.Query
import Seri.SMT.Solver (Solver)

import Seri.Elaborate

-- | Given a Seri expression of type Query a,
-- returns the Seri expression of type a which results from running the query.
run :: (Solver s) => Exp -> Query s Exp
run e = do
    env <- envQ
    case elabwhnf env e of
        (AppE (VarE (Sig n _)) [arg]) | n == name "Seri.SMT.SMT.query" -> do
            res <- query (realize arg)
            case res of 
                Satisfiable arg' -> do
                    return $ AppE (ConE (Sig (name "Satisfiable") (AppT (ConT (name "Answer")) (typeof arg)))) [arg']
                Unsatisfiable -> return $ ConE (Sig (name "Unsatisfiable") (AppT (ConT (name "Answer")) (typeof arg)))
                _ -> return $ ConE (Sig (name "Unknown") (AppT (ConT (name "Answer")) (typeof arg)))
        (VarE (Sig n (AppT _ t))) | n == name "Seri.SMT.SMT.free" -> free t
        (AppE (VarE (Sig n _)) [p]) | n == name "Seri.SMT.SMT.assert" -> do
            assert p
            return unitE
        (AppE (VarE (Sig n _)) [q]) | n == name "Seri.SMT.SMT.queryS" -> queryS $ run q
        (AppE (VarE (Sig n _)) [x]) | n == name "Seri.SMT.SMT.return_query" -> return x
        (AppE (VarE (Sig n _)) [x, f]) | n == name "Seri.SMT.SMT.bind_query" -> do
          result <- run x
          run (AppE f [result])
        (AppE (VarE (Sig n _)) [x, y]) | n == name "Seri.SMT.SMT.nobind_query" -> do
          run x
          run y
        x -> error $ "unknown Query: " ++ pretty x

