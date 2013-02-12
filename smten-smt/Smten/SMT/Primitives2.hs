
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Smten.SMT.Primitives2 (
    symbolicEH, de_symbolicEH,
    smtEH, de_smtEH,
    smtrefEH, de_smtrefEH,
    smt2Ps,
    return_SymbolicP, fail_SymbolicP, bind_SymbolicP, nobind_SymbolicP,
    return_SMTP, fail_SMTP, bind_SMTP, nobind_SMTP,
    free_IntegerP, free_BoolP, free_BitP,
    assertP, readSMTRefP, queryP, nestP, commitP,
    runSMT
    ) where

import Debug.Trace

import Data.Functor((<$>))
import Data.Maybe

import Smten.Type
import Smten.Name
import Smten.Lit
import Smten.Sig
import Smten.ExpH
import Smten.Ppr
import Smten.SMT.Query
import Smten.SMT.Yices.Yices1
import Smten.SMT.Yices.Yices2
import Smten.SMT.STP.STP
import Smten.Prim
import Smten.SMT.Primitives (queryEH, de_queryEH)

data Solver = Yices1 | Yices2 | STP
    deriving (Eq, Show)

derive_SmtenT ''Solver
derive_SmtenEH ''Solver

newtype Symbolic a = Symbolic { symbolic_query :: Query a }
    deriving (Monad, Functor)

instance (SmtenT1 Symbolic) where
    smtenT1 _ = conT (name "Symbolic")

symbolicEH :: Symbolic ExpH -> ExpH
symbolicEH = queryEH . symbolic_query

de_symbolicEH :: ExpH -> Maybe (Symbolic ExpH)
de_symbolicEH x = Symbolic <$> de_queryEH x

instance (SmtenEH a) => SmtenEH (Symbolic a) where
    smtenEH x = symbolicEH (smtenEH <$> x)
    de_smtenEH e = do
        q <- de_symbolicEH e
        return $ fromMaybe (error "de_smtenEH Symbolic") . de_smtenEH <$> q


newtype SMTRef a = SMTRef { smtref_query :: Query a }
    deriving (Monad, Functor)

instance (SmtenT1 SMTRef) where
    smtenT1 _ = conT (name "SMTRef")

smtrefEH :: SMTRef ExpH -> ExpH
smtrefEH = queryEH . smtref_query

de_smtrefEH :: ExpH -> Maybe (SMTRef ExpH)
de_smtrefEH x = SMTRef <$> de_queryEH x

instance (SmtenEH a) => SmtenEH (SMTRef a) where
    smtenEH x = smtrefEH (smtenEH <$> x)
    de_smtenEH e = do
        q <- de_smtrefEH e
        return $ fromMaybe (error "de_smtenEH SMTRef") . de_smtenEH <$> q

newtype SMT a = SMT { smt_query :: Query a }
    deriving (Monad, Functor)

instance (SmtenT1 SMT) where
    smtenT1 _ = conT (name "SMT")

smtEH :: SMT ExpH -> ExpH
smtEH = queryEH . smt_query

de_smtEH :: ExpH -> Maybe (SMT ExpH)
de_smtEH x = SMT <$> de_queryEH x

instance (SmtenEH a) => SmtenEH (SMT a) where
    smtenEH x = smtEH (smtenEH <$> x)
    de_smtenEH e = do
        q <- de_smtEH e
        return $ fromMaybe (error "de_smtenEH SMT") . de_smtenEH <$> q

smt2Ps :: [Prim]
smt2Ps = [
    return_SymbolicP, fail_SymbolicP, bind_SymbolicP, nobind_SymbolicP,
    return_SMTP, fail_SMTP, bind_SMTP, nobind_SMTP,
    free_IntegerP, free_BoolP, free_BitP,
    assertP, queryP, readSMTRefP, nestP, commitP,
    runSMT
    ]

return_SMTP :: Prim
return_SMTP = unaryP "Smten.SMT.Symbolic.return_smt" (return :: ExpH -> SMT ExpH)

fail_SMTP :: Prim
fail_SMTP = unaryP "Smten.SMT.Symbolic.fail_smt" (fail :: String -> SMT ExpH)

bind_SMTP :: Prim
bind_SMTP = binaryP "Smten.SMT.Symbolic.bind_smt" ((>>=) :: SMT ExpH -> (ExpH -> SMT ExpH) -> SMT ExpH)

nobind_SMTP :: Prim
nobind_SMTP = binaryP "Smten.SMT.Symbolic.nobind_smt" ((>>) :: SMT ExpH -> SMT ExpH -> SMT ExpH)

return_SymbolicP :: Prim
return_SymbolicP = unaryP "Smten.SMT.Symbolic.return_symbolic" (return :: ExpH -> Symbolic ExpH)

fail_SymbolicP :: Prim
fail_SymbolicP = unaryP "Smten.SMT.Symbolic.fail_symbolic" (fail :: String -> Symbolic ExpH)

bind_SymbolicP :: Prim
bind_SymbolicP = binaryP "Smten.SMT.Symbolic.bind_symbolic" ((>>=) :: Symbolic ExpH -> (ExpH -> Symbolic ExpH) -> Symbolic ExpH)

nobind_SymbolicP :: Prim
nobind_SymbolicP = binaryP "Smten.SMT.Symbolic.nobind_symbolic" ((>>) :: Symbolic ExpH -> Symbolic ExpH -> Symbolic ExpH)


free_helper :: Type -> Symbolic ExpH
free_helper t =
  let Just (_, t') = de_appT t
  in Symbolic $ free t'

free_IntegerP :: Prim
free_IntegerP = nullaryTP "Smten.SMT.Symbolic.__prim_free_Integer" free_helper

free_BoolP :: Prim
free_BoolP = nullaryTP "Smten.SMT.Symbolic.__prim_free_Bool" free_helper

free_BitP :: Prim
free_BitP = nullaryTP "Smten.SMT.Symbolic.__prim_free_Bit" free_helper

assertP :: Prim
assertP = unaryP "Smten.SMT.Symbolic.assert" (Symbolic . assert)

queryP :: Prim
queryP =
  let f :: Symbolic ExpH -> SMT ExpH
      f arg = SMT . queryS $ do
        v <- symbolic_query arg
        res <- query (realize v)
        let ta = AppT (ConT (name "Maybe")) (typeof v)
        case res of
            Satisfiable arg' -> return $ identify $ \id -> ConEH id (name "Just") ta [arg']
            Unsatisfiable -> return $ identify $ \id -> ConEH id (name "Nothing") ta []
            _ -> return $ errorEH ta "query: Unknown"
  in unaryP "Smten.SMT.Symbolic.query" f

readSMTRefP :: Prim
readSMTRefP =
  let f :: SMTRef ExpH -> Symbolic ExpH
      f = Symbolic . smtref_query
  in unaryP "Smten.SMT.Symbolic.readSMTRef" f

commitP :: Prim
commitP =
  let f :: Symbolic ExpH -> SMT (SMTRef ExpH)
      f arg = SMT $ do
        v <- symbolic_query arg
        return (SMTRef $ return v)
  in unaryP "Smten.SMT.Symbolic.commit" f

nestP :: Prim
nestP = unaryP "Smten.SMT.Symbolic.nest" (queryS :: Query ExpH -> Query ExpH)

runSMT :: Prim
runSMT =
  let f :: Solver -> Maybe FilePath -> SMT ExpH -> IO ExpH
      f solver dbg q = do
        s <- case solver of
                Yices1 -> yices1
                Yices2 -> yices2
                STP -> stp
        runQuery (RunOptions dbg s) (smt_query q)
  in binaryP "Smten.SMT.Symbolic.runSMT" f

