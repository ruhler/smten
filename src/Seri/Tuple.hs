
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Tuple where

import Seri.Declarations
import Seri.IR
import Seri.Quoter
import Seri.Typed

declval "Tuple2"
    [t| (SeriType a, SeriType b) => Typed Exp (a -> b -> (a, b)) |]
    [e| conE "Tuple2" |] []

instance SeriType2 (,) where
    seritype2 _ = ConT "Tuple2"

[s| 
    fst :: (a, b) -> a
    fst (x, _) = x

    snd :: (a, b) -> b
    snd (_, y) = y
|]

