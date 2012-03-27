
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Seri.IR (ir)
    where

import Language.Haskell.TH

import Seri.Elaborate
import Seri.Ppr
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

-- clause3 function myx
--  Returns a clause for the function definition of the form:
--      function a b (myx x) = function a b x
clause3 :: Name -> Name -> Q Clause
clause3 func myx 
  = let a = mkName "a"
        b = mkName "b"
        x = mkName "x"
    in clause [varP a, varP b, conP myx [varP x]] (normalB
            (appE (appE (appE (varE func) (varE a)) (varE b)) (varE x))) []

-- impl n f cs
-- Convenience function. See use examples for why it's convenient
--
--  n - the name of a function
--  f - a function which produces a clause of 'n' given a 'f' and a type
--      constructor. e.g. clause1, clause3
--  cs - a list of type constructors
--
-- Return a function definition for 'n' consisting of a clause for each
-- type constructor.
impl :: Name -> (Name -> Name -> Q Clause) -> [Con] -> Q Dec
impl n f cs = funD n (map (\(NormalC cn _) -> f n cn) cs)

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

       elab_impl = impl 'elaborate clause1 cons
       redu_impl = impl 'reduce clause3 cons
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
   let typeof_impl = impl 'typeof clause1 cons
       typecheck_impl = impl 'typecheck clause1 cons
       checkvars_impl = impl 'checkvars clause3 cons
       inst = instanceD (return []) (appT (appT (conT ''TypeCheck) (conT mytype)) (conT my)) [typeof_impl, typecheck_impl, checkvars_impl]
    in sequence [inst]

-- derive_ppr
--  Given a data declaration of the form
--      data MyFoo = MyA A | MyB B | ...
--
--  Derive an instance of Ppr MyFoo
--  Of the form
--      instance Ppr MyFoo where
--          ppr (MyA x) = ppr x
--          ppr (MyB x) = ppr x
--          ...
derive_ppr :: Dec -> Q [Dec]
derive_ppr (DataD _ my _ cons _) =
  let ppr_impl = impl 'ppr clause1 cons
      inst = instanceD (return []) (appT (conT ''Ppr) (conT my)) [ppr_impl]
   in sequence [inst]


-- ir type exp
--  Given the name of your top level MyType and MyExp data types,
--      generate boilerplate code for injection and elaboration.
ir :: Name -> Name -> Q [Dec]
ir tname ename = do
    TyConI t <- reify tname
    tpprs <- derive_ppr t
    tinjs <- injections t

    TyConI e <- reify ename
    epprs <- derive_ppr e
    einjs <- injections e
    elab <- derive_elaborate e

    tchk <- derive_typecheck tname e 

    return $ concat [tpprs, tinjs, epprs, einjs, elab, tchk]

