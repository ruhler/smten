
module Smten.Runtime.Debug (
    Debug, dbgRender,
    dbgOp, dbgCase, dbgPrim, dbgError, dbgVar, dbgCon,
    ) where

import Text.PrettyPrint.HughesPJ

type Debug = Doc

dbgOp :: String -> Debug -> Debug -> Debug
dbgOp op a b = a <+> text op <+> b

dbgVar :: String -> Debug
dbgVar = text

tabwidth :: Int
tabwidth = 2

dbgCase :: String -> Debug -> Debug -> Debug -> Debug
dbgCase k x y n
 = text "case" <+> x <+> text "of" <+> text "{"
     $+$ nest tabwidth (vcat [
             text k <+> text "->" <+> y,
             text "_" <+> text "->" <+> n
          ]) $+$ text "}"

dbgPrim :: Debug
dbgPrim = text "?Prim?"

dbgError :: String -> Debug
dbgError s = text "error" <+> text (show s)

dbgCon :: String -> [Debug] -> Debug
dbgCon k xs = sep ((text k) : xs)

dbgRender :: Debug -> String
dbgRender = render

