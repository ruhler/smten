
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Tuple where

import Seri.IR
import Seri.Typed
import Seri.Declarations
import Seri.Quoter

declcon "(,)" [t| forall a b. (a -> b -> (a, b)) |]

instance SeriType2 (,) where
    seritype2 _ = ConT "(,)"

[s| 
    fst :: (a, b) -> a
    fst (x, _) = x

    snd :: (a, b) -> b
    snd (_, y) = y
|]

declcon "(,,)" [t| forall a b c. (a -> b -> c -> (a, b, c)) |]

instance SeriType3 (,,) where
    seritype3 _ = ConT "(,,)"
