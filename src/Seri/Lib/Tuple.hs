
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Tuple where

import Seri.IR
import Seri.FrontEnd.Typed
import Seri.FrontEnd.Declarations.User
import Seri.FrontEnd.Polymorphic
import Seri.FrontEnd.Quoter

decltycon 2 ''(,)
declcon "(,)" [t| forall a b. (a -> b -> (a, b)) |]


[s| 
    fst :: (a, b) -> a
    fst (x, _) = x

    snd :: (a, b) -> b
    snd (_, y) = y
|]

decltycon 3 ''(,,)
declcon "(,,)" [t| forall a b c. (a -> b -> c -> (a, b, c)) |]

