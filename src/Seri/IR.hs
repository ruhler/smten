
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Seri.IR (ir)
    where

import Language.Haskell.TH

import Seri.Elaborate


-- injections
--  Given a data declaration of the form
--      data MyExp = MyA A | MyB B | ...
--
--  Derive instances of Inject for all A, B, ...
--  Of the form
--      instance Inject A MyExp where
--          inject = MyA
--          unject (MyA x) = Just x
--          unject _ = Nothing
injections :: Dec -> Q [Dec]
injections (DataD _ name _ cons _) =
    let injcon :: Name -> Name -> Type -> Q [Dec]
        injcon my a ta =
            let x = mkName "x"
                inject_impl = funD 'inject [
                    clause [] (normalB (conE a)) []
                    ]
                unject_impl = funD 'unject [
                    clause [conP a [varP x]] (normalB (appE [e| Just |] (varE x))) [],
                    clause [wildP] (normalB ([e| Nothing |])) []
                    ]
                inst = instanceD (return []) (appT (appT (conT ''Inject) (return ta)) (conT my)) [inject_impl, unject_impl]
            in sequence [inst]
    in do
        decs <- mapM (\(NormalC cn [(_, ct)]) -> injcon name cn ct) cons
        return $ concat decs

-- selfelab
--  Given a data declaration of the form
--      data MyExp = MyA A | MyB B | ...
--
--  Derive an instance of Elaborate MyExp MyExp
--  Of the form
--      instance Elaborate MyExp MyExp where
--          elaborate (MyA x) = elaborate x
--          elaborate (MyB x) = elaborate x
--          ...
--          reduce n v (MyA x) = reduce n v x
--          reduce n v (MyB x) = reduce n v x
selfelab :: Dec -> Q [Dec]
selfelab (DataD _ my _ cons _) =
   let x = mkName "x"
       n = mkName "n"
       v = mkName "v"

       elabclause :: Name -> Q Clause
       elabclause a = clause [conP a [varP x]] (normalB
            (appE (varE 'elaborate) (varE x))) []

       reduclause :: Name -> Q Clause
       reduclause a = clause [varP n, varP v, conP a [varP x]] (normalB
            (appE (appE (appE (varE 'reduce) (varE n)) (varE v)) (varE x))) []

       elab_impl = funD 'elaborate (map (\(NormalC cn _) -> elabclause cn) cons)
       redu_impl = funD 'reduce (map (\(NormalC cn _) -> reduclause cn) cons)
       inst = instanceD (return []) (appT (appT (conT ''Elaborate) (conT my)) (conT my)) [elab_impl, redu_impl]
    in sequence [inst]


-- ir type exp
--  Given the name of your top level MyType and MyExp data types,
--      generate boilerplate code for injection and elaboration.
ir :: Name -> Name -> Q [Dec]
ir tname ename = do
    TyConI t <- reify tname
    tinjs <- injections t

    TyConI e <- reify ename
    einjs <- injections e
    esele <- selfelab e

    return $ tinjs ++ einjs ++ esele

