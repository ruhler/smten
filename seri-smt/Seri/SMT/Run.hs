
{-# LANGUAGE PatternGuards #-}

-- | Run a Seri Exp of type Query in the haskell Query monad.
module Seri.SMT.Run (run) where

import Debug.Trace

import Seri.Type
import Seri.ExpH
import Seri.Name
import Seri.Sig
import Seri.Ppr
import Seri.SMT.Query

import Seri.Elaborate

-- | Given a Seri expression of type Query a,
-- returns the Seri expression of type a which results from running the query.
run :: ExpH -> Query ExpH
run e = do
    env <- envQ
    case elaborate env e of
        e' | Just arg <- de_appv1 (name "Seri.SMT.SMT.query") e' -> do
            res <- query (realize arg)
            case res of 
                Satisfiable arg' ->
                    let tsat = arrowsT [typeof arg, AppT (ConT (name "Answer")) (typeof arg)]
                        result = appsEH (conEH (Sig (name "Satisfiable") tsat)) [arg']
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
          run (appsEH f [result])
        e' | Just (x, y) <- de_appv2 (name "Seri.SMT.SMT.nobind_query") e' -> do
          run x
          run y
        e' -> error $ "unknown Query: " ++ pretty e'

