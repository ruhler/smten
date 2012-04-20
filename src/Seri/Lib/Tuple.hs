
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Tuple where

import Seri.IR
import Seri.Typed
import Seri.Declarations.User
import Seri.Quoter

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

