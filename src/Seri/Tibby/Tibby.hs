
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
|]

instance Monad Module where
    return = error $ "Module return"
    (>>=) = error $ "Module >>="

class Interface a b where
    form :: Module (a, b)

class Interface1 a b where
    form1 :: Module (a c, b c)

instance (Interface1 a b) => (Interface (a c) (b c)) where
    form = form1

[s|
    data Action a = Action
        deriving(Show, Eq)
|]

instance Monad Action where
    return = error $ "Action return"
    (>>=) = error $ "Action >>="

[s|
    data Program =
          Atom (Action ())
        | Seq [Program]
        | Par [Program]
        | Loop Program
     deriving(Eq, Show)

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

instance Interface1 Put Put' where
    form1 = error $ "Put, Put' form1"

declprim "make" [t| forall a b . (Interface a b) => (b -> Module Program) -> Module a |] 
declprim "return" [t| forall a m . (Monad m) => a -> m a |]
declprim ">>" [t| forall a b m . (Monad m) => m a -> m b -> m b |]
declprim ">>=" [t| forall a b m . (Monad m) => m a -> (a -> m b) -> m b |]


