
{-# LANGUAGE NoImplicitPrelude #-}

module Smten.Smten.List (List__(..)) where

data List__ a = Nil__ | Cons__ a (List__ a)

