
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Tibby.Tibby where

import qualified Language.Haskell.TH as TH

import Seri
import Seri.Lib.Prelude

data Module a = Module
decltype ''Module

instance Monad Module where
    return = error $ "Module return"
    (>>=) = error $ "Module >>="

class Interface a b where
    form :: Module (a, b)

class Interface1 a b where
    form1 :: Module (a c, b c)

instance (Interface1 a b) => (Interface (a c) (b c)) where
    form = form1

data Action a = Action
    deriving(Show, Eq)
decltype ''Action

instance Monad Action where
    return = error $ "Action return"
    (>>=) = error $ "Action >>="

data Program =
      Atom (Action ())
    | Seq [Program]
    | Par [Program]
    | Loop Program
 deriving(Eq, Show)
decltype ''Program

[s|
    action :: Action () -> Program
    action = Atom

    seq :: [Program] -> Program
    seq = Seq

    par :: [Program] -> Program
    par = Par

    loop :: Program -> Program
    loop = Loop
|]

data Put a = Put {
    put :: a -> Action ()
}
decltype ''Put

data Put' a = Put' {
    put' :: Action a
}
decltype ''Put'

[s|
    get :: Put' a -> Action a
    get = put'
|]

instance Interface1 Put Put' where
    form1 = error $ "Put, Put' form1"

declprim "make" [t| (SeriType a, SeriType b, Interface a b) => Typed Exp ((b -> Module Program) -> Module a) |] 
declprim "return" [t| (SeriType a, SeriType1 m, Monad m) => Typed Exp (a -> m a) |]
declprim ">>" [t| (SeriType a, SeriType b, SeriType1 m, Monad m) => Typed Exp (m a -> m b -> m b) |]
declprim ">>=" [t| (SeriType a, SeriType b, SeriType1 m, Monad m) => Typed Exp (m a -> (a -> m b) -> m b) |]

data Terminating a = Terminating {
    islast :: Bool,
    value :: a
}

decltype ''Terminating

