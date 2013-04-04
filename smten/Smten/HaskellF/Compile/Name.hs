
{-# LANGUAGE PatternGuards #-}

module Smten.HaskellF.Compile.Name (
    hsName, hsTyName, nmn, nmk,
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
hsName n
 | Just i <- de_tupleN n = H.mkName $ "Tuple" ++ show i ++ "__"
 | n == unitN = H.mkName $ "Unit__"
 | n == name ":" = H.mkName $ "Cons__"
 | n == name "[]" = H.mkName $ "Nil__"
hsName n =
  let dequalify :: String -> String
      dequalify n = 
        case break (== '.') n of
            (n', []) -> n'
            (_, ".") -> "."
            (_, n') -> dequalify (tail n')
      symify :: String -> String
      symify s = if issymbol s then "(" ++ s ++ ")" else s
  in H.mkName . symify . dequalify . unname $ n

issymbol :: String -> Bool
issymbol ('(':_) = False
issymbol "[]" = False
issymbol (h:_) = not $ isAlphaNum h || h == '_'

-- | Translate a type constructor name to haskell.
-- Remaps builtin prelude type constructor names as appropriate.
hsTyName :: Name -> H.Name
hsTyName n
 | n == unitN = H.mkName "Unit__"
 | Just x <- de_tupleN n = H.mkName $ "Tuple" ++ show x ++ "__"
 | n == name "[]" = H.mkName "List__"
 | otherwise = hsName n

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

