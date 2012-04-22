
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

-- class Interface a b where
--     form :: Module (a, b)
-- 
-- class Interface1 a b where
--     form1 :: Module (a c, b c)
-- 
-- instance (Interface1 a b) => (Interface (a c) (b c)) where
--     form = form1
-- 
-- instance Monad Action where
--     return = error $ "Action return"
--     (>>=) = error $ "Action >>="
-- 
-- 
-- instance Interface1 Put Put' where
--     form1 = error $ "Put, Put' form1"
-- 
