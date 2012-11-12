
{-# LANGUAGE PatternGuards #-}

-- | Run a Seri Exp of type Query in the haskell Query monad.
module Seri.SMT.Run (run) where

import Debug.Trace

import Seri.Lambda hiding (free, query)
import Seri.Type.Sugar
import Seri.SMT.Query
import Seri.SMT.Solver (Solver)

import Seri.Elaborate hiding (query)

-- | Given a Seri expression of type Query a,
-- returns the Seri expression of type a which results from running the query.
run :: (Solver s) => ExpH -> Query s ExpH
run e = do
    env <- envQ
    case elabwhnf env e of
        e' | Just arg <- de_appv1 (name "Seri.SMT.SMT.query") e' -> do
            res <- query (realize arg)
            case res of 
                Satisfiable arg' ->
                    let tsat = arrowsT [typeof arg, AppT (ConT (name "Answer")) (typeof arg)]
                        result = appEH (conEH (Sig (name "Satisfiable") tsat)) [arg']
                    in return result
                Unsatisfiable -> return $ conEH (Sig (name "Unsatisfiable") (AppT (ConT (name "Answer")) (typeof arg)))
                _ -> return $ conEH (Sig (name "Unknown") (AppT (ConT (name "Answer")) (typeof arg)))
        e' | Just (Sig n (AppT _ t)) <- de_varEH e'
           , n == name "Seri.SMT.SMT.__prim_free" -> free t
        e' | Just p <- de_appv1 (name "Seri.SMT.SMT.assert") e' -> do
            assert p
            return unitEH
        e' | Just q <- de_appv1 (name "Seri.SMT.SMT.queryS") e' -> queryS $ run q
        e' | Just x <- de_appv1 (name "Seri.SMT.SMT.return_query") e' -> return x
        e' | Just (x, f) <- de_appv2 (name "Seri.SMT.SMT.bind_query") e' -> do
          result <- run x
          run (appEH f [result])
        e' | Just (x, y) <- de_appv2 (name "Seri.SMT.SMT.nobind_query") e' -> do
          run x
          run y
        e' -> error $ "unknown Query: " ++ pretty e'

