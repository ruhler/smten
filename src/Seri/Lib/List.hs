

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.List where

import Seri
import Seri.Lib.Integer

decltycon 1 ''[]
declcon ":" [t| forall a. a -> [a] -> [a] |]
declcon "[]" [t| forall a. [a] |]

[s| 
    head :: [a] -> a
    head (x:_) = x

    tail :: [a] -> [a]
    tail (_:xs) = xs

    length :: [a] -> Integer
    length [] = 0
    length (x:xs) = 1 + length xs
|]

