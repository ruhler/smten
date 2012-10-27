
{-# LANGUAGE PatternGuards #-}

-- | Run a Seri Exp of type IO in the haskell IO monad.
module Seri.IO.Run (run) where

import Seri.Lambda
import Seri.Elaborate
import Seri.Enoch.Enoch
import Seri.Enoch.Seriables
import Seri.SMT.Query
import qualified Seri.SMT.Run
import Seri.SMT.Yices.Yices2

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
        (AppE (VarE (Sig n _)) [debug, query])
            | n == name "Seri.IO.SMT.runYices2"
            , Just dbg <- unpack (TExp debug :: TExp (Maybe String))
            -> runQuery (RunOptions dbg True) env (yices2 $ Seri.SMT.Run.run query)
        (AppE (VarE (Sig n _)) [x]) | n == name "Seri.IO.IO.return_io" -> return x
        (AppE (VarE (Sig n _)) [x, f]) | n == name "Seri.IO.IO.bind_io" -> do
          result <- run env x
          run env (AppE f [result])
        (AppE (VarE (Sig n _)) [x, y]) | n == name "Seri.IO.IO.nobind_io" -> do
          run env x
          run env y
        x -> error $ "unknown IO: " ++ pretty x

