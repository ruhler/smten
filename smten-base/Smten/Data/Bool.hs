
{-# LANGUAGE NoImplicitPrelude #-}

module Smten.Data.Bool (
    Bool(False,True), (&&), (||), not, otherwise,
    ifThenElse,
  ) where

infixr 3 &&
infixr 2 ||

data Bool = False | True

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

