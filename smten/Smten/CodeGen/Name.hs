
{-# LANGUAGE PatternGuards #-}

module Smten.CodeGen.Name (
    modprefix, 
    nameCG, qnameCG, tynameCG, qtynameCG,
    casenmCG, qcasenmCG,
    ) where

import qualified Language.Haskell.TH.Syntax as H
import Data.Char (isAlphaNum)

import Smten.Name

modprefix :: Name -> String
modprefix = modprefixs . unname

modprefixs :: String -> String
modprefixs n = "Smten.Lib." ++ n

issymbol :: String -> Bool
issymbol ('(':_) = False
issymbol "[]" = False
issymbol (h:_) = not $ isAlphaNum h || h == '_'

-- | Generate code for an unqualified variable or data constructor name.
nameCG :: Name -> H.Name
nameCG nm 
  | nm == unitN = H.mkName "Unit__"
  | nm == nilN = H.mkName "Nil__"
  | nm == consN = H.mkName "Cons__"
  | Just i <- de_tupleN nm = H.mkName $ "Tuple" ++ show i ++ "__"
  | issymbol (unname $ unqualified nm) = H.mkName ("(" ++ unname (unqualified nm) ++ ")")
  | otherwise = H.mkName (unname $ unqualified nm)

-- qualified variable or data constructor name.
qnameCG :: Name -> H.Name
qnameCG nm
  | nm == unitN = H.mkName $ modprefixs "Prelude.Unit__"
  | nm == nilN = H.mkName $ modprefixs "Prelude.Nil__"
  | nm == consN = H.mkName $ modprefixs "Prelude.Cons__"
  | Just i <- de_tupleN nm = H.mkName $ modprefixs ("Prelude.Tuple" ++ show i ++ "__")
  | isqualified nm && issymbol (unname $ unqualified nm) = H.mkName ("(" ++ modprefix nm ++ ")")
  | isqualified nm = H.mkName (modprefix nm)
  | issymbol (unname $ unqualified nm) = H.mkName ("(" ++ unname nm ++ ")")
  | otherwise = H.mkName (unname nm)

-- | Generate code for an unqualified type constructor name.
tynameCG :: Name -> H.Name
tynameCG nm
  | nm == unitN = H.mkName "Unit__"
  | nm == listN = H.mkName "List__"
  | Just i <- de_tupleN nm = H.mkName $ "Tuple" ++ show i ++ "__"
  | otherwise = H.mkName (unname $ unqualified nm)

-- | qualified type constructor name.
qtynameCG :: Name -> H.Name
qtynameCG nm
  | nm == unitN = H.mkName $ modprefixs "Prelude.Unit__"
  | nm == listN = H.mkName $ modprefixs "Prelude.List__"
  | Just i <- de_tupleN nm = H.mkName $ modprefixs ("Prelude.Tuple" ++ show i ++ "__")
  | isqualified nm = H.mkName (modprefix nm)
  | otherwise = H.mkName (unname nm)

casenmCG :: Name -> H.Name
casenmCG n
 | n == unitN = H.mkName "__caseUnit__"
 | n == nilN = H.mkName "__caseNil__"
 | n == consN = H.mkName "__caseCons__"
 | Just i <- de_tupleN n = H.mkName $ "__caseTuple" ++ show i ++ "__"
 | otherwise = H.mkName ("__case" ++ (unname $ unqualified n))

qcasenmCG :: Name -> H.Name
qcasenmCG n
 | n == unitN = H.mkName $ modprefixs "Prelude.__caseUnit__"
 | n == nilN = H.mkName $ modprefixs "Prelude.__caseNil__"
 | n == consN = H.mkName $ modprefixs "Prelude.__caseCons__"
 | Just i <- de_tupleN n = H.mkName $ modprefixs ("Prelude.__caseTuple" ++ show i ++ "__")
 | isqualified n = H.mkName $ modprefix n
 | otherwise = H.mkName ("__case" ++ (unname n))

