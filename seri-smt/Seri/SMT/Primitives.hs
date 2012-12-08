
{-# LANGUAGE PatternGuards #-}

module Seri.SMT.Primitives (
    queryEH, de_queryEH,
    smtPs,
    return_QueryP, fail_QueryP, bind_QueryP, nobind_QueryP,
    freeP, queryP, assertP, querySP,
    runYices1P, runYices2P, runSTPP,
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

queryEH :: Query ExpH -> ExpH
queryEH = litEH . dynamicL

de_queryEH :: ExpH -> Maybe (Query ExpH)
de_queryEH e = de_litEH e >>= de_dynamicL

instance SeriT1 Query where
    seriT1 _ = conT (name "Query")

instance (SeriEH a) => SeriEH (Query a) where
    seriEH x = queryEH (seriEH <$> x)
    de_seriEH e = do
        q <- de_queryEH e
        return $ fromMaybe (error "de_seriEH Query") . de_seriEH <$> q

smtPs :: [Prim]
smtPs = [
    return_QueryP, fail_QueryP,
    bind_QueryP, nobind_QueryP,
    freeP, queryP, assertP, querySP,
    runYices1P, runYices2P, runSTPP
    ]

return_QueryP :: Prim
return_QueryP = unaryP "Seri.SMT.SMT.return_query" (return :: ExpH -> Query ExpH)

fail_QueryP :: Prim
fail_QueryP = unaryP "Seri.SMT.SMT.fail_query" (fail :: String -> Query ExpH)

bind_QueryP :: Prim
bind_QueryP = binaryP "Seri.SMT.SMT.bind_query" ((>>=) :: Query ExpH -> (ExpH -> Query ExpH) -> Query ExpH)

nobind_QueryP :: Prim
nobind_QueryP = binaryP "Seri.SMT.SMT.nobind_query" ((>>) :: Query ExpH -> Query ExpH -> Query ExpH)

freeP :: Prim
freeP =
  let f :: Type -> Query ExpH
      f t =
        let Just (_, t') = de_appT t
        in free t'
  in nullaryTP "Seri.SMT.SMT.__prim_free" f

assertP :: Prim
assertP = unaryP "Seri.SMT.SMT.assert" assert

queryP :: Prim
queryP =
  let f :: ExpH -> Query ExpH
      f arg = do
        res <- query (realize arg)
        let ta = AppT (ConT (name "Answer")) (typeof arg)
        case res of
            Satisfiable arg' -> return $ ConEH (name "Satisfiable") ta [arg']
            Unsatisfiable -> return $ ConEH (name "Unsatisfiable") ta []
            _ -> return $ ConEH (name "Unknown") ta []
  in unaryP "Seri.SMT.SMT.query" f

querySP :: Prim
querySP = unaryP "Seri.SMT.SMT.queryS" (queryS :: Query ExpH -> Query ExpH)

runYices1P :: Prim
runYices1P =
  let f :: Maybe FilePath -> Query ExpH -> IO ExpH
      f dbg q = do
        y1 <- yices1
        runQuery (RunOptions dbg y1) q
  in binaryP "Seri.SMT.SMT.runYices1" f

runYices2P :: Prim
runYices2P =
  let f :: Maybe FilePath -> Query ExpH -> IO ExpH
      f dbg q = do
        y2 <- yices2
        runQuery (RunOptions dbg y2) q
  in binaryP "Seri.SMT.SMT.runYices2" f

runSTPP :: Prim
runSTPP =
  let f :: Maybe FilePath -> Query ExpH -> IO ExpH
      f dbg q = do
        s <- stp
        runQuery (RunOptions dbg s) q
  in binaryP "Seri.SMT.SMT.runSTP" f

