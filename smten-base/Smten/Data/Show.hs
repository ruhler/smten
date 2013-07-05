
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Data.Show (
    ShowS, Show(..), shows, showChar, showString, showParen,
    ) where

import Smten.Smten.Base
import Smten.Data.Bool
import Smten.Data.Function
import Smten.Data.List0
import Smten.Data.Show0

type ShowS = String -> String

class Show a where
    showsPrec :: Int -> a -> ShowS
    showsPrec _ x s = show x ++ s

    show :: a -> String
    show x = showsPrec 0 x ""

    showList :: [a] -> ShowS
    showList [] = showString "[]"
    showList (x:xs) = showChar '[' . shows x . showl xs
       where showl [] = showChar ']'
             showl (x:xs) = showChar ',' . shows x . showl xs

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

instance Show Int where
    showsPrec = int_showsPrec

instance Show Char where
    showsPrec = char_showsPrec
    showList = char_showList

instance (Show a) => Show [a] where
    showsPrec p = showList

instance Show Integer where
    showsPrec = integer_showsPrec

