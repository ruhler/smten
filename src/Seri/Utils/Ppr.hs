
{-# LANGUAGE FlexibleInstances #-}

module Seri.Utils.Ppr (
        module Text.PrettyPrint.HughesPJ,
        Ppr(..),
    ) where

import Text.PrettyPrint.HughesPJ
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Ppr as TH

class Ppr a where
    ppr :: a -> Doc

instance Ppr [TH.Dec] where
    ppr ds = text . show $ TH.ppr ds

instance Ppr TH.Exp where
    ppr e = text . show $ TH.ppr e

