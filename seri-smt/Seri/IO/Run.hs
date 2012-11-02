
{-# LANGUAGE PatternGuards #-}

-- | Run a Seri Exp of type IO in the haskell IO monad.
module Seri.IO.Run (run) where

import Seri.Lambda
import Seri.Elaborate
import Seri.Enoch.Enoch
import Seri.Enoch.Seriables
import Seri.SMT.Query
import qualified Seri.SMT.Run
import Seri.SMT.Yices.Yices1
import Seri.SMT.Yices.Yices2
import Seri.SMT.STP.STP

-- | Given a Seri expression of type IO a,
-- returns the Seri expression of type a which results from running the IO
-- computation.
run :: Env -> Exp -> IO Exp
run env e = do
    case elabwhnf env e of
        (AppE (VarE (Sig n _)) [arg]) | n == name "Prelude.putStr" -> do
            case deStringE (elabwhnf env arg) of
                Just str -> putStr str
                Nothing -> error $ "putStr: expected string, got: " ++ pretty arg
            return unitE
        (AppE (VarE (Sig n _)) [debug, query])
            | n == name "Seri.SMT.SMT.runYices1"
            , Just dbg <- unpack (TExp debug :: TExp (Maybe String))
            -> runQuery (RunOptions dbg True) env (yices1 $ Seri.SMT.Run.run query)
        (AppE (VarE (Sig n _)) [debug, query])
            | n == name "Seri.SMT.SMT.runYices2"
            , Just dbg <- unpack (TExp debug :: TExp (Maybe String))
            -> runQuery (RunOptions dbg True) env (yices2 $ Seri.SMT.Run.run query)
        (AppE (VarE (Sig n _)) [debug, query])
            | n == name "Seri.SMT.SMT.runSTP"
            , Just dbg <- unpack (TExp debug :: TExp (Maybe String))
            -> runQuery (RunOptions dbg True) env (stp $ Seri.SMT.Run.run query)
        (AppE (VarE (Sig n _)) [x]) | n == name "Prelude.return_io" -> return x
        (AppE (VarE (Sig n _)) [x, f]) | n == name "Prelude.bind_io" -> do
          result <- run env x
          run env (AppE f [result])
        (AppE (VarE (Sig n _)) [x, y]) | n == name "Prelude.nobind_io" -> do
          run env x
          run env y
        x -> error $ "unknown IO: " ++ pretty x

