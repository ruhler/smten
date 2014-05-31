
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Base.Prelude (($!)) where

infixr 0 $!

($!) :: (a -> b) -> a -> b
f $! x = let !vx = x in f vx

