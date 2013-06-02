
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.CodeGen.Ppr () where

import qualified Language.Haskell.TH.PprLib as H
import qualified Text.PrettyPrint.HughesPJ as HPJ

import Smten.Ppr

instance Ppr H.Doc where
  ppr d =
    let hpj = H.to_HPJ_Doc d
        hstyle = (HPJ.style { HPJ.lineLength = maxBound })
        str = {-# SCC "RenderHaskellF" #-} HPJ.renderStyle hstyle hpj
    in text str 
    
