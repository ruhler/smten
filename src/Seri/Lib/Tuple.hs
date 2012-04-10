
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Tuple where

import Seri

declval "(,)"
    [t| (SeriType a, SeriType b) => Typed Exp (a -> b -> (a, b)) |]
    [e| conE "(,)" |] []

instance SeriType2 (,) where
    seritype2 _ = ConT "(,)"

[s| 
    fst :: (a, b) -> a
    fst (x, _) = x

    snd :: (a, b) -> b
    snd (_, y) = y
|]

declval "(,,)"
    [t| (SeriType a, SeriType b, SeriType c)
            => Typed Exp (a -> b -> c -> (a, b, c)) |]
    [e| conE "(,,)" |] []

instance SeriType3 (,,) where
    seritype3 _ = ConT "(,,)"
