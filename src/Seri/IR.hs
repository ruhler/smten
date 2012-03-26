
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Seri.IR (ir)
    where

import Language.Haskell.TH

import Seri.Elaborate (Inject(..))


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

ir :: Name -> Q [Dec]
ir name = do
    TyConI x <- reify name
    injections x

