
{-# LANGUAGE NoImplicitPrelude #-}

module Smten.Data.Bool (
    Bool(..), (&&), (||), not, otherwise,
    ifThenElse,
  ) where

import Smten.Data.Bool0

infixr 3 &&
infixr 2 ||

(&&) :: Bool -> Bool -> Bool
(&&) True x = x
(&&) False _ = False

(||) :: Bool -> Bool -> Bool
(||) True _ = True
(||) False x = x

not :: Bool -> Bool
not True = False
not False = True

otherwise :: Bool
otherwise = True

ifThenElse :: Bool -> a -> a -> a
ifThenElse p x y = 
    case p of
      True -> x 
      False -> y

