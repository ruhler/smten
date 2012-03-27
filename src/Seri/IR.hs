
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Seri.IR (ir)
    where

import Language.Haskell.TH

import Seri.Elaborate
import Seri.TypeCheck


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

-- clause1 function myx
--  Returns a clause for the function definition of the form:
--      function (myx x) = function x
clause1 :: Name -> Name -> Q Clause
clause1 func a 
  = let x = mkName "x"
    in clause [conP a [varP x]] (normalB (appE (varE func) (varE x))) []


-- derive_elaborate
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
derive_elaborate :: Dec -> Q [Dec]
derive_elaborate (DataD _ my _ cons _) =
   let x = mkName "x"
       n = mkName "n"
       v = mkName "v"

       reduclause :: Name -> Q Clause
       reduclause a = clause [varP n, varP v, conP a [varP x]] (normalB
            (appE (appE (appE (varE 'reduce) (varE n)) (varE v)) (varE x))) []

       elab_impl = funD 'elaborate (map (\(NormalC cn _) -> clause1 'elaborate cn) cons)
       redu_impl = funD 'reduce (map (\(NormalC cn _) -> reduclause cn) cons)
       inst = instanceD (return []) (appT (appT (conT ''Elaborate) (conT my)) (conT my)) [elab_impl, redu_impl]
    in sequence [inst]

-- derive_typecheck
--  Given the name of a type MyType and data declarations of the form
--      data MyExp = MyA A | MyB B | ...
--
--  Derive an instance of TypeCheck MyType MyExp
--  Of the form
--      instance TypeCheck MyType MyExp where
--          typeof (MyA x) = typeof x
--          typeof (MyB x) = typeof x
--          ...
--          checkvars n t (MyA x) = checkvars n t x
--          checkvars n t (MyB x) = checkvars n t x
--          ...
--          typecheck (MyA x) = typecheck x
--          typecheck (MyB x) = typecheck x
--          ...
derive_typecheck :: Name -> Dec -> Q [Dec]
derive_typecheck mytype (DataD _ my _ cons _) =
   let x = mkName "x"
       n = mkName "n"
       t = mkName "t"

       checkvarsclause :: Name -> Q Clause
       checkvarsclause a = clause [varP n, varP t, conP a [varP x]] (normalB
            (appE (appE (appE (varE 'checkvars) (varE n)) (varE t)) (varE x))) []

       typeof_impl = funD 'typeof (map (\(NormalC cn _) -> clause1 'typeof cn) cons)
       typecheck_impl = funD 'typecheck (map (\(NormalC cn _) -> clause1 'typecheck cn) cons)
       checkvars_impl = funD 'checkvars (map (\(NormalC cn _) -> checkvarsclause cn) cons)
       inst = instanceD (return []) (appT (appT (conT ''TypeCheck) (conT mytype)) (conT my)) [typeof_impl, typecheck_impl, checkvars_impl]
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
    elab <- derive_elaborate e

    tchk <- derive_typecheck tname e 

    return $ tinjs ++ einjs ++ elab ++ tchk

