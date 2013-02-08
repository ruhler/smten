
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.SMT.Primitives2 (
    symbolicEH, de_symbolicEH,
    smtEH, de_smtEH,
    smt2Ps,
    return_SymbolicP, fail_SymbolicP, bind_SymbolicP, nobind_SymbolicP,
    return_SMTP, fail_SMTP, bind_SMTP, nobind_SMTP,
    free_IntegerP, free_BoolP, free_BitP,
    assertP, queryP, nestP, commitP,
    runSMT
    ) where

import Debug.Trace

import Data.Functor((<$>))
import Data.Maybe

import Seri.Type
import Seri.Name
import Seri.Lit
import Seri.Sig
import Seri.ExpH
import Seri.Ppr
import Seri.SMT.Query
import Seri.SMT.Yices.Yices1
import Seri.SMT.Yices.Yices2
import Seri.SMT.STP.STP
import Seri.Prim
import Seri.SMT.Primitives (queryEH, de_queryEH)

data Solver = Yices1 | Yices2 | STP
    deriving (Eq, Show)

derive_SeriT ''Solver
derive_SeriEH ''Solver

newtype Symbolic a = Symbolic { symbolic_query :: Query a }
    deriving (Monad, Functor)

instance (SeriT1 Symbolic) where
    seriT1 _ = conT (name "Symbolic")

symbolicEH :: Symbolic ExpH -> ExpH
symbolicEH = queryEH . symbolic_query

de_symbolicEH :: ExpH -> Maybe (Symbolic ExpH)
de_symbolicEH x = Symbolic <$> de_queryEH x

instance (SeriEH a) => SeriEH (Symbolic a) where
    seriEH x = symbolicEH (seriEH <$> x)
    de_seriEH e = do
        q <- de_symbolicEH e
        return $ fromMaybe (error "de_seriEH Symbolic") . de_seriEH <$> q

newtype SMT a = SMT { smt_query :: Query a }
    deriving (Monad, Functor)

instance (SeriT1 SMT) where
    seriT1 _ = conT (name "SMT")

smtEH :: SMT ExpH -> ExpH
smtEH = queryEH . smt_query

de_smtEH :: ExpH -> Maybe (SMT ExpH)
de_smtEH x = SMT <$> de_queryEH x

instance (SeriEH a) => SeriEH (SMT a) where
    seriEH x = smtEH (seriEH <$> x)
    de_seriEH e = do
        q <- de_smtEH e
        return $ fromMaybe (error "de_seriEH SMT") . de_seriEH <$> q

smt2Ps :: [Prim]
smt2Ps = [
    return_SymbolicP, fail_SymbolicP, bind_SymbolicP, nobind_SymbolicP,
    return_SMTP, fail_SMTP, bind_SMTP, nobind_SMTP,
    free_IntegerP, free_BoolP, free_BitP,
    assertP, queryP, nestP, commitP,
    runSMT
    ]

return_SMTP :: Prim
return_SMTP = unaryP "Seri.SMT.Symbolic.return_smt" (return :: ExpH -> SMT ExpH)

fail_SMTP :: Prim
fail_SMTP = unaryP "Seri.SMT.Symbolic.fail_smt" (fail :: String -> SMT ExpH)

bind_SMTP :: Prim
bind_SMTP = binaryP "Seri.SMT.Symbolic.bind_smt" ((>>=) :: SMT ExpH -> (ExpH -> SMT ExpH) -> SMT ExpH)

nobind_SMTP :: Prim
nobind_SMTP = binaryP "Seri.SMT.Symbolic.nobind_smt" ((>>) :: SMT ExpH -> SMT ExpH -> SMT ExpH)

return_SymbolicP :: Prim
return_SymbolicP = unaryP "Seri.SMT.Symbolic.return_symbolic" (return :: ExpH -> Symbolic ExpH)

fail_SymbolicP :: Prim
fail_SymbolicP = unaryP "Seri.SMT.Symbolic.fail_symbolic" (fail :: String -> Symbolic ExpH)

bind_SymbolicP :: Prim
bind_SymbolicP = binaryP "Seri.SMT.Symbolic.bind_symbolic" ((>>=) :: Symbolic ExpH -> (ExpH -> Symbolic ExpH) -> Symbolic ExpH)

nobind_SymbolicP :: Prim
nobind_SymbolicP = binaryP "Seri.SMT.Symbolic.nobind_symbolic" ((>>) :: Symbolic ExpH -> Symbolic ExpH -> Symbolic ExpH)


free_helper :: Type -> Symbolic ExpH
free_helper t =
  let Just (_, t') = de_appT t
  in Symbolic $ free t'

free_IntegerP :: Prim
free_IntegerP = nullaryTP "Seri.SMT.Symbolic.__prim_free_Integer" free_helper

free_BoolP :: Prim
free_BoolP = nullaryTP "Seri.SMT.Symbolic.__prim_free_Bool" free_helper

free_BitP :: Prim
free_BitP = nullaryTP "Seri.SMT.Symbolic.__prim_free_Bit" free_helper

assertP :: Prim
assertP = unaryP "Seri.SMT.Symbolic.assert" (Symbolic . assert)

queryP :: Prim
queryP =
  let f :: Symbolic ExpH -> SMT ExpH
      f arg = SMT . queryS $ do
        v <- symbolic_query arg
        res <- query (realize v)
        let ta = AppT (ConT (name "Answer")) (typeof v)
        case res of
            Satisfiable arg' -> return $ identify $ \id -> ConEH id (name "Satisfiable") ta [arg']
            Unsatisfiable -> return $ identify $ \id -> ConEH id (name "Unsatisfiable") ta []
            _ -> return $ identify $ \id -> ConEH id (name "Unknown") ta []
  in unaryP "Seri.SMT.Symbolic.query" f

commitP :: Prim
commitP =
  let f :: Symbolic ExpH -> SMT (Symbolic ExpH)
      f arg = SMT $ do
        v <- symbolic_query arg
        return (Symbolic $ return v)
  in unaryP "Seri.SMT.Symbolic.commit" f

nestP :: Prim
nestP = unaryP "Seri.SMT.Symbolic.nest" (queryS :: Query ExpH -> Query ExpH)

runSMT :: Prim
runSMT =
  let f :: Solver -> Maybe FilePath -> SMT ExpH -> IO ExpH
      f solver dbg q = do
        s <- case solver of
                Yices1 -> yices1
                Yices2 -> yices2
                STP -> stp
        runQuery (RunOptions dbg s) (smt_query q)
  in binaryP "Seri.SMT.Symbolic.runSMT" f

