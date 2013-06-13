
{-# LANGUAGE FlexibleContexts #-}

module Smten.Runtime.Numeric (
    valueof, Integer,
    ) where

import Smten.Numeric
import qualified Smten.Runtime.SmtenHS as S

instance S.SmtenHS0 (NumT n) where
    realize0 _ _ = numeric
    cases0 _ = S.concrete numeric
    primitive0 _ _ = numeric
    error0 = error "TODO: error0 for NumT"

instance S.SmtenHS2 (:+:) where
    realize2 _ _ = numeric
    cases2 _ = S.concrete numeric
    primitive2 _ _ = numeric
    error2 = error "TODO: error0 for :+:"

instance S.SmtenHS2 (:-:) where
    realize2 _ _ = numeric
    cases2 _ = S.concrete numeric
    primitive2 _ _ = numeric
    error2 = error "TODO: error0 for :-:"

instance S.SmtenHS2 (:*:) where
    realize2 _ _ = numeric
    cases2 _ = S.concrete numeric
    primitive2 _ _ = numeric
    error2 = error "TODO: error0 for :*:"

