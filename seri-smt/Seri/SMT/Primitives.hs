
{-# LANGUAGE PatternGuards #-}

module Seri.SMT.Primitives (
    __prim_freeEH, __prim_queryEH, __prim_assertEH, __prim_querySEH,
    __prim_return_QueryEH, __prim_bind_QueryEH, __prim_nobind_QueryEH,
    __prim_fail_QueryEH,
    __prim_runYices1EH, __prim_runYices2EH, __prim_runSTPEH,
    ) where

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

queryEH :: Query ExpH -> ExpH
queryEH = litEH . dynamicL

de_queryEH :: ExpH -> Maybe (Query ExpH)
de_queryEH e = de_litEH e >>= de_dynamicL

__prim_freeEH :: Type -> ExpH
__prim_freeEH t
 | Just (_, t') <- de_appT t = queryEH (free t')

__prim_assertEH :: ExpH -> ExpH
__prim_assertEH p = queryEH $ assert p >> return unitEH

__prim_queryEH :: ExpH -> ExpH
__prim_queryEH arg = queryEH $ do
    res <- query (realize arg)
    case res of
        Satisfiable arg' ->
            let tsat = arrowsT [typeof arg, AppT (ConT (name "Answer")) (typeof arg)]
                result = appsEH (conEH (Sig (name "Satisfiable") tsat)) [arg']
            in return result
        Unsatisfiable -> return $ conEH (Sig (name "Unsatisfiable") (AppT (ConT (name "Answer")) (typeof arg)))
        _ -> return $ conEH (Sig (name "Unknown") (AppT (ConT (name "Answer")) (typeof arg)))

__prim_querySEH :: ExpH -> ExpH
__prim_querySEH q
 | Just v <- de_queryEH q = queryEH $ queryS v

__prim_return_QueryEH :: ExpH -> ExpH
__prim_return_QueryEH a = queryEH (return a)

__prim_bind_QueryEH :: ExpH -> ExpH -> ExpH
__prim_bind_QueryEH x f = queryEH $ do
    let Just xq = de_queryEH x
    r <- xq
    case appEH f r of
        q | Just fq <- de_queryEH q -> fq
          | otherwise -> error $ "expecting Query, got: " ++ pretty q
        

__prim_nobind_QueryEH :: ExpH -> ExpH -> ExpH
__prim_nobind_QueryEH a b
 | Just aq <- de_queryEH a
 , Just bq <- de_queryEH b = queryEH $ aq >> bq

__prim_fail_QueryEH :: ExpH -> ExpH
__prim_fail_QueryEH a
 | Just v <- de_stringEH a = queryEH $ fail v

__prim_runYices1EH :: ExpH -> ExpH -> ExpH
__prim_runYices1EH debug query
 | Just dbg <- de_seriEH debug
 , Just q <- de_queryEH query = ioEH $ do
     y1 <- yices1
     runQuery (RunOptions dbg y1) q

__prim_runYices2EH :: ExpH -> ExpH -> ExpH
__prim_runYices2EH debug query
 | Just dbg <- de_seriEH debug
 , Just q <- de_queryEH query = ioEH $ do
     y2 <- yices2
     runQuery (RunOptions dbg y2) q
    

__prim_runSTPEH :: ExpH -> ExpH -> ExpH
__prim_runSTPEH debug query
 | Just dbg <- de_seriEH debug
 , Just q <- de_queryEH query = ioEH $ do
     s <- stp
     runQuery (RunOptions dbg s) q

