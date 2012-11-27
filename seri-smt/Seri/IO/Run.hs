
{-# LANGUAGE PatternGuards #-}

-- | Run a Seri Exp of type IO in the haskell IO monad.
module Seri.IO.Run (run) where

import Seri.Elaborate
import Seri.SMT.Query
import qualified Seri.SMT.Run
import Seri.SMT.Yices.Yices1
import Seri.SMT.Yices.Yices2
import Seri.SMT.STP.STP
import Seri.Name
import Seri.ExpH
import Seri.Dec
import Seri.Ppr

-- | Given a Seri expression of type IO a,
-- returns the Seri expression of type a which results from running the IO
-- computation.
run :: EnvH -> ExpH -> IO ExpH
run env e = do
    case elaborate env e of
        e' | Just arg <- de_appv1 (name "Prelude.putChar") e' -> do
            case de_charEH (elaborate env arg) of
                Just c -> putChar c
                Nothing -> error $ "putChar: expected Char, got: " ++ pretty (elaborate env arg)
            return unitEH
        e' | Just (debug, query) <- de_appv2 (name "Seri.SMT.SMT.runYices1") e'
           , Just dbg <- de_seriEH debug -> do  
                y1 <- yices1
                runQuery (RunOptions dbg y1) env (Seri.SMT.Run.run query)
        e' | Just (debug, query) <- de_appv2 (name "Seri.SMT.SMT.runYices2") e'
           , Just dbg <- de_seriEH debug -> do
                y2 <- yices2
                runQuery (RunOptions dbg y2) env (Seri.SMT.Run.run query)
        e' | Just (debug, query) <- de_appv2 (name "Seri.SMT.SMT.runSTP") e'
           , Just dbg <- de_seriEH debug -> do
                s <- stp
                runQuery (RunOptions dbg s) env (Seri.SMT.Run.run query)
        e' | Just x <- de_appv1 (name "Prelude.return_io") e' -> return x
        e' | Just (x, f) <- de_appv2 (name "Prelude.bind_io") e' -> do
              result <- run env x
              run env (appsEH f [result])
        e' | Just (x, y) <- de_appv2 (name "Prelude.nobind_io") e' -> do
              run env x
              run env y
        x -> error $ "unknown IO: " ++ pretty x

