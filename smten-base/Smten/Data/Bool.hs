
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O #-}

module Smten.Data.Bool (
    Bool(..), (&&), (||), not, otherwise,
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

