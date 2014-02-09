
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.GHC.Show (
    ShowS, Show(..), showList__, shows, showChar, showString, showParen,
    ) where

import Smten.Smten.Base
import Smten.Data.Bool
import Smten.Data.Function
import Smten.Data.List0
import Smten.Data.Show0

type ShowS = String -> String

class Show a where
    showsPrec :: Int -> a -> ShowS
    show :: a -> String
    showList :: [a] -> ShowS

    showsPrec _ x s = show x ++ s
    show x= shows x ""
    showList ls s = showList__ shows ls s

showList__ :: (a -> ShowS) -> [a] -> ShowS
showList__ _ [] s = "[]" ++ s
showList__ showx (x:xs) s = '[' : showx x (showl xs)
  where showl [] = ']' : s
        showl (y:ys) = ',' : showx y (showl ys)

shows :: (Show a) => a -> ShowS
shows = showsPrec 0

showChar :: Char -> ShowS
showChar = (:)

showString :: String -> ShowS
showString = (++)

showParen :: Bool -> ShowS -> ShowS
showParen b p =
  case b of
    True -> showChar '(' . p . showChar ')' 
    False -> p

instance Show () where
    show () = "()"

instance Show Bool where
    show True = "True"
    show False = "False"

instance Show Int where
    showsPrec = int_showsPrec

instance Show Char where
    showsPrec = char_showsPrec
    showList = char_showList

instance (Show a) => Show [a] where
    showsPrec p = showList

instance Show Integer where
    showsPrec = integer_showsPrec

instance (Show a, Show b) => Show (a, b) where
    show (a, b) = "(" ++ show a ++ "," ++ show b ++ ")"


