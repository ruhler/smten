
{-# LANGUAGE PatternGuards #-}

-- | Run a Seri Exp of type Query in the haskell Query monad.
module Seri.SMT.Run (run) where

import Debug.Trace

import Seri.Lambda hiding (free, query)
import Seri.SMT.Query
import Seri.SMT.Solver (Solver)

import Seri.Elaborate

-- Match an application of the variable with given name to a single argument.
-- Returns the argument.
de_appv1 :: Name -> Exp -> Maybe Exp
de_appv1 n (AppE (VarE (Sig nm _)) [x]) | n == nm = Just x
de_appv1 _ _ = Nothing

de_appv2 :: Name -> Exp -> Maybe (Exp, Exp)
de_appv2 n (AppE (VarE (Sig nm _)) [x, y]) | n == nm = Just (x, y)
de_appv2 _ _ = Nothing

de_varE :: Exp -> Maybe Sig
de_varE (VarE s) = Just s
de_varE _ = Nothing

-- | Given a Seri expression of type Query a,
-- returns the Seri expression of type a which results from running the query.
run :: (Solver s) => Exp -> Query s Exp
run e = do
    env <- envQ
    case elabwhnf env e of
        e' | Just arg <- de_appv1 (name "Seri.SMT.SMT.query") e' -> do
            res <- query (realize arg)
            case res of 
                Satisfiable arg' ->
                    let tsat = arrowsT [typeof arg, AppT (ConT (name "Answer")) (typeof arg)]
                        result = AppE (ConE (Sig (name "Satisfiable") tsat)) [arg']
                    in return result
                Unsatisfiable -> return $ ConE (Sig (name "Unsatisfiable") (AppT (ConT (name "Answer")) (typeof arg)))
                _ -> return $ ConE (Sig (name "Unknown") (AppT (ConT (name "Answer")) (typeof arg)))
        e' | Just (Sig n (AppT _ t)) <- de_varE e'
           , n == name "Seri.SMT.SMT.__prim_free" -> free t
        e' | Just p <- de_appv1 (name "Seri.SMT.SMT.assert") e' -> do
            assert p
            return unitE
        e' | Just q <- de_appv1 (name "Seri.SMT.SMT.queryS") e' -> queryS $ run q
        e' | Just x <- de_appv1 (name "Seri.SMT.SMT.return_query") e' -> return x
        e' | Just (x, f) <- de_appv2 (name "Seri.SMT.SMT.bind_query") e' -> do
          result <- run x
          run (appE f [result])
        e' | Just (x, y) <- de_appv2 (name "Seri.SMT.SMT.nobind_query") e' -> do
          run x
          run y
        e' -> error $ "unknown Query: " ++ pretty e'

