
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
qnameCG nm = do
  case (nameModule_maybe nm) of
     Just mn -> addimport ("Smten.Lib." ++ moduleNameString (moduleName mn))
     _ -> return ()
  let nm' = trans $ nameString nm
      base = unqualified nm'
      qlfn = qualification nm'
      qlfn' = if null qlfn then "" else "Smten.Lib." ++ qlfn ++ "."
      full = qlfn' ++ base
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
trans "GHC.Types.:" = "Prelude.CONS"
trans "GHC.Types.[]" = "Prelude.NIL"
trans "GHC.Tuple.()" = "Prelude.UNIT"
trans "GHC.Tuple.(,)" = "Prelude.TUPLE2"
trans "GHC.Tuple.(,,)" = "Prelude.TUPLE3"
trans "GHC.Tuple.(,,,)" = "Prelude.TUPLE4"
trans s = s

-- | Return the qualification on the given name.
-- For example: qualification "Foo.Bar.sludge" returns "Foo.Bar"
qualification :: String -> String
qualification n =
  case (span (/= '.') n) of
    (_, []) -> ""
    ([], ['.']) -> ""
    (x, '.':xs) -> 
        let qxs = qualification xs
        in if null qxs
             then x
             else qualified x qxs

qualified :: String -> String -> String
qualified a b = a ++ "." ++ b

