
module Smten.Runtime.Debug (
    Debug, dbgRender,
    dbgOp, dbgCase, dbgText, dbgError, dbgVar, dbgCon, dbgApp,
    ) where

import Text.PrettyPrint.HughesPJ

type Debug = Doc

dbgOp :: String -> Debug -> Debug -> Debug
dbgOp op a b = a <+> text op <+> b

dbgApp :: Debug -> Debug -> Debug
dbgApp a b = a <+> b

dbgVar :: String -> Debug
dbgVar = text

tabwidth :: Int
tabwidth = 2

dbgCase :: String -> Debug -> Debug -> Debug -> Debug
dbgCase k x y n
 | k == "True"
     = text "if" <+> x $+$
        nest tabwidth (vcat [text "then" <+> y,
                             text "else" <+> n])
 | otherwise
     = text "case" <+> x <+> text "of" <+> text "{"
         $+$ nest tabwidth (vcat [
                 text k <+> text "->" <+> y,
                 text "_" <+> text "->" <+> n
              ]) $+$ text "}"

dbgText :: String -> Debug
dbgText = text

dbgError :: String -> Debug
dbgError s = text "error" <+> text (show s)

dbgCon :: String -> [Debug] -> Debug
dbgCon k xs = sep ((text k) : xs)

dbgRender :: Debug -> String
dbgRender = {-# SCC "DebugRender" #-} render

