
{-# LANGUAGE PatternGuards #-}

module Smten.HaskellF.Compile.Name (
    hsName, hsqName, hsTyName, hsqTyName,
    nmn, nmk, qcasenm, casenm, qconnm, connm, symnm, hfpre,
    ) where

import qualified Language.Haskell.TH.Syntax as H

import Data.Char(isAlphaNum)

import Smten.Name
import Smten.Type

import Smten.HaskellF.Compile.Kind

-- | Translate a data constructor or variable name to an unqualified haskell
-- name.
-- Remaps builtin prelude data constructor names as appropriate.
-- Adds parenthesis around operator names as appropriate.
hsName :: Name -> H.Name
hsName n =
  let symify :: String -> String
      symify s = if issymbol s then "(" ++ s ++ ")" else s
  in H.mkName . symify . hfnm $ n

-- Same as hsName, only produces the qualified version of the name.
hsqName :: Name -> H.Name
hsqName n =
  let symify :: String -> String
      symify s = if issymbol (hfnm n) then "(" ++ s ++ ")" else s
  in H.mkName . symify . hfqnm $ n

issymbol :: String -> Bool
issymbol ('(':_) = False
issymbol "[]" = False
issymbol (h:_) = not $ isAlphaNum h || h == '_'

-- | Translate a type constructor name to an unqualified haskell name.
-- Remaps builtin prelude type constructor names as appropriate.
hsTyName :: Name -> H.Name
hsTyName n = H.mkName $ hftynm n

-- | Translate a type constructor name to a qualified haskell name.
-- Remaps builtin prelude type constructor names as appropriate.
hsqTyName :: Name -> H.Name
hsqTyName n = H.mkName $ hfqtynm n

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

-- Convert a Smten data constructor name to it's corresponding unqualified
-- HaskellF data constructor name.
hfnm :: Name -> String
hfnm n
 | Just i <- de_tupleN n = "Tuple" ++ show i ++ "__"
 | n == unitN = "Unit__"
 | n == consN = "Cons__"
 | n == nilN = "Nil__"
 | otherwise = unname (unqualified n)

-- Convert a Smten data constructor name to it's corresponding qualified
-- HaskellF data constructor name.
hfqnm :: Name -> String
hfqnm n
 | Just i <- de_tupleN n = "Smten.Lib.Prelude.Tuple" ++ show i ++ "__"
 | n == unitN = "Smten.Lib.Prelude.Unit__"
 | n == consN = "Smten.Lib.Prelude.Cons__"
 | n == nilN = "Smten.Lib.Prelude.Nil__"
 | isqualified n = unname (hfpre n)
 | otherwise = unname n

-- Convert a Smten type constructor name to it's corresponding unqualified
-- HaskellF type constructor name.
hftynm :: Name -> String
hftynm n
 | n == unitN = "Unit__"
 | Just x <- de_tupleN n = "Tuple" ++ show x ++ "__"
 | n == nilN = "List__"
 | otherwise = unname (unqualified n)

-- Convert a Smten type constructor name to it's corresponding qualified
-- HaskellF type constructor name.
hfqtynm :: Name -> String
hfqtynm n
 | n == unitN = "Smten.Lib.Prelude.Unit__"
 | n == boolN = "Smten.Lib.Prelude.Bool"
 | n == maybeN = "Smten.Lib.Prelude.Maybe"
 | Just x <- de_tupleN n = "Smten.Lib.Prelude.Tuple" ++ show x ++ "__"
 | n == nilN = "Smten.Lib.Prelude.List__"
 | otherwise = unname (qualified (name "Smten.Lib") n)

-- Given the name of a data constructor, return the unqualified name of the
-- function for doing a case match against the constructor.
casenm :: Name -> H.Name
casenm n = H.mkName $ "__case" ++ hfnm n

connm :: Name -> H.Name
connm n = H.mkName $ "__mk" ++ hfnm n

-- Given the name of a data constructor, return the qualified name of the
-- function for doing a case match against the constructor.
qcasenm :: Name -> H.Name
qcasenm n =
  let qn = name $ hfqnm n
      qlf = unname $ qualification qn
      unq = unname $ unqualified qn
  in H.mkName $ qlf ++ ".__case" ++ unq

qconnm :: Name -> H.Name
qconnm n =
  let qn = name $ hfqnm n
      qlf = unname $ qualification qn
      unq = unname $ unqualified qn
  in H.mkName $ qlf ++ ".__mk" ++ unq


-- | Given the name of a type constructor, return the unqualified symbolic data
-- constructor associated with it.
symnm :: Name -> H.Name
symnm n = H.mkName $ hftynm n ++ "__s"

-- Prefix the given name to put it into the HaskellF library module namespace.
hfpre :: Name -> Name
hfpre = qualified (name "Smten.Lib")

