
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Seri.Elaborate (Inject(..), Elaborate(..))
    where

class Inject a b where
    inject :: a -> b
    unject :: b -> Maybe a

instance Inject a a where
    inject = id
    unject = Just

class (Show a, Show b, Inject a b) => Elaborate a b where
    elaborate :: a -> b

