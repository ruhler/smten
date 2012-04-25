
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Tibby.Tibby where

import qualified Language.Haskell.TH as TH

import Seri
import Seri.Lib.Prelude

[s|
    data Module a = Module

    data Action a = Action

    data Program =
          Atom (Action ())
        | Seq [Program]
        | Par [Program]
        | Loop Program

    action :: Action () -> Program
    action = Atom

    seq :: [Program] -> Program
    seq = Seq

    par :: [Program] -> Program
    par = Par

    loop :: Program -> Program
    loop = Loop

    data Put a = Put {
        put :: a -> Action ()
    }

    data Put' a = Put' {
        put' :: Action a
    }

    get :: Put' a -> Action a
    get = put'

    data Terminating a = Terminating {
        islast :: Bool,
        value :: a
    }

|]

class Interface a b where
    form :: Module (a, b)

declclass ''Interface
 
class Interface1 m n where
    form1 :: Module (m a, n a)
declclass ''Interface1

declprim "return_action" [t| forall a . a -> Action a |]
declprim "nobind_action" [t| forall a b . Action a -> Action b -> Action b |]
declprim "bind_action" [t| forall a b . Action a -> (a -> Action b) -> Action b |]
declprim "fail_action" [t| forall a . String -> Action a |]

declprim "form1_put" [t| forall a . Module (Put a, Put' a) |]

[s|
    instance Monad Action where
        return = return_action
        (>>=) = bind_action
        (>>) = nobind_action
        fail = fail_action

    instance Interface1 Put Put' where
        form1 = form1_put
    
|]
 
-- instance (Interface1 m n) => (Interface (m a) (m b)) where
--     form = form1
-- 

