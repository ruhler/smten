

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.List where

import Seri

declval ":"
    [t| (SeriType a) => Typed Exp (a -> [a] -> [a]) |]
    [e| conE ":" |] []

declval "[]"
    [t| (SeriType a) => Typed Exp [a] |]
    [e| conE "[]" |] []

instance SeriType1 [] where
    seritype1 _ = ConT "[]"

[s| 
    head :: [a] -> a
    head (x:_) = x

    tail :: [a] -> [a]
    tail (_:xs) = xs
|]

