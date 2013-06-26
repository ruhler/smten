
module Smten.Plugin.Name (
    nameCG, qnameCG,
    ) where

import Data.Char

import GhcPlugins
import Smten.Plugin.CG
import qualified Smten.Plugin.Output.Syntax as S

-- Generate code for an unqualified name.
nameCG :: Name -> CG S.Name
nameCG nm =
 let full = trans $ nameString nm
     base = unqualified full
     sym = if issymbol base then "(" ++ base ++ ")" else base
 in return sym

-- Generate code for a qualified name.
qnameCG :: Name -> CG S.Name
qnameCG nm
 | nameString nm == "GHC.Integer.Type.Integer" = return $ "Prelude.Integer"
 | otherwise = do
  case (nameModule_maybe nm) of
     Just mn -> addimport (moduleName mn)
     _ -> return ()
  let full = trans $ nameString nm
      base = unqualified full
      sym = if issymbol base then "(" ++ full ++ ")" else full
  return $ sym

-- | Return the unqualified part of the given name.
-- For example: unqualified "Foo.Bar.sludge" returns "sludge"
unqualified :: String -> String
unqualified n = 
  case (span (/= '.') n) of
    (_, []) -> n
    ([], ['.']) -> "."
    (_, '.':xs) -> unqualified xs

nameString :: Name -> String
nameString nm = renderWithStyle tracingDynFlags (ppr nm) defaultUserStyle

issymbol :: String -> Bool
issymbol ('(':_) = False
issymbol "[]" = False
issymbol (h:_) = not $ isAlphaNum h || h == '_'

trans :: String -> String
trans "GHC.Types.:" = ":"
trans "GHC.Types.[]" = "[]"
trans "GHC.Tuple.()" = "()"
trans "GHC.Tuple.(,)" = "(,)"
trans "GHC.Tuple.(,,)" = "(,,)"
trans "GHC.Tuple.(,,,)" = "(,,,)"
trans "GHC.Integer.Type.Integer" = "Prelude.Integer"
trans s = s

