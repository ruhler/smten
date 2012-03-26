
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Seri.Elaborate (Inject(..), Elaborate(..))
    where

import Seri.Name

class Inject a b where
    inject :: a -> b
    unject :: b -> Maybe a

instance Inject a a where
    inject = id
    unject = Just

class (Inject a b) => Elaborate a b where
    elaborate :: a -> b

    -- reduce n v exp
    -- Perform beta reduction in the exp, replacing occurances of varaible n
    -- with v.
    reduce :: Name -> b -> a -> b

