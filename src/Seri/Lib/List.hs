

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.List where

import Seri

declcon ":" [t| forall a. a -> [a] -> [a] |]
declcon "[]" [t| forall a. [a] |]

instance SeriType1 [] where
    seritype1 _ = ConT "[]"

[s| 
    head :: [a] -> a
    head (x:_) = x

    tail :: [a] -> [a]
    tail (_:xs) = xs
|]

