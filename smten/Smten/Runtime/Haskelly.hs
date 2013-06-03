
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Runtime.Haskelly (Haskelly(..), tohs', Poly(..)) where

import Data.Maybe(fromMaybe)

class Haskelly h s where
    frhs :: h -> s
    tohs :: s -> Maybe h

tohs' :: (Haskelly h s) => s -> h
tohs' = fromMaybe (error "tohs'") . tohs

instance (Haskelly ha sa, Haskelly hb sb) => Haskelly (ha -> hb) (sa -> sb) where
    frhs hf sx = frhs $ hf (tohs' sx)
    tohs sf = return (\hx -> tohs' $ sf (frhs hx))

newtype Poly a = Poly a

instance Haskelly (Poly s) s where
    frhs (Poly x) = x
    tohs x = return (Poly x)

