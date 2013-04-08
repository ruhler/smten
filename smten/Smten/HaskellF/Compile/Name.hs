
{-# LANGUAGE PatternGuards #-}

module Smten.HaskellF.Compile.Name (
    hsName, hsTyName, nmn, nmk,
    casenm, symnm, hfpre,
    ) where

import qualified Language.Haskell.TH.Syntax as H

import Data.Char(isAlphaNum)

import Smten.Name
import Smten.Type

import Smten.HaskellF.Compile.Kind

-- | Translate a data constructor or variable name to haskell.
-- Remaps builtin prelude data constructor names as appropriate.
-- Adds parenthesis around operator names as appropriate.
--
-- TODO: Here we just drop the qualified part of the name.
-- This is a hack, requiring there are no modules which define an entity of
-- the same name (unlikely...). Really we should form a proper haskell name
-- for whatever this name is used for (varid, conid)
hsName :: Name -> H.Name
hsName n =
  let dequalify :: String -> String
      dequalify n = 
        case break (== '.') n of
            (n', []) -> n'
            (_, ".") -> "."
            (_, n') -> dequalify (tail n')
      symify :: String -> String
      symify s = if issymbol s then "(" ++ s ++ ")" else s
  in H.mkName . symify . dequalify . hfnm $ n

issymbol :: String -> Bool
issymbol ('(':_) = False
issymbol "[]" = False
issymbol (h:_) = not $ isAlphaNum h || h == '_'

-- | Translate a type constructor name to haskell.
-- Remaps builtin prelude type constructor names as appropriate.
hsTyName :: Name -> H.Name
hsTyName n = hsName $ name (hftynm n)

-- Make a name with the number after it.
-- If the number is 0, the original name is returned.
--
-- This is the case for names of classes and methods that work on different
-- kinds, where kind 0 is standard, and kind N /= 0 is written explicitly in
-- the name.
nmn :: String -> Integer -> H.Name
nmn s 0 = H.mkName s
nmn s n = H.mkName $ s ++ show n

nmk :: String -> Kind -> H.Name
nmk s k = nmn s (knum k)

-- Convert a Smten data constructor name to it's corresponding HaskellF data
-- constructor name.
hfnm :: Name -> String
hfnm n
 | Just i <- de_tupleN n = "Tuple" ++ show i ++ "__"
 | n == unitN = "Unit__"
 | n == consN = "Cons__"
 | n == nilN = "Nil__"
 | otherwise = unname (unqualified n)

-- Convert a Smten type constructor name to it's corresponding HaskellF type
-- constructor name.
hftynm :: Name -> String
hftynm n
 | n == unitN = "Unit__"
 | Just x <- de_tupleN n = "Tuple" ++ show x ++ "__"
 | n == nilN = "List__"
 | otherwise = unname (unqualified n)

-- Given the name of a data constructor, return the name of the function for
-- doing a case match against the constructor.
casenm :: Name -> H.Name
casenm n = H.mkName $ "__case" ++ hfnm n

-- | Given the name of a type constructor, return the symbolic data
-- constructor associated with it.
symnm :: Name -> H.Name
symnm n = H.mkName $ hftynm n ++ "__s"

-- Prefix the given name to put it into the HaskellF library module namespace.
hfpre :: Name -> Name
hfpre = qualified (name "Smten.Lib")

