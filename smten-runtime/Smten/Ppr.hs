
module Smten.Ppr (
    Ppr(..), pretty, tabwidth,
    module Text.PrettyPrint.HughesPJ,
    ) where

import Text.PrettyPrint.HughesPJ

class Ppr a where
    ppr :: a -> Doc

tabwidth :: Int
tabwidth = 2

-- | Print an object very prettily.
pretty :: (Ppr a) => a -> String
pretty = render . ppr

