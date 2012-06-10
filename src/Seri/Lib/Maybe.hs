
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Maybe where

import Seri
import Seri.Lib.Monad
import Seri.Lib.Tuple

decltype ''Maybe

[s|
    instance Monad Maybe where
        fail _ = Nothing
        return = Just
        (>>) a b = a >>= \_ -> b
        (>>=) m f = 
            case m of
                Just x -> f x
                Nothing -> Nothing
|]

