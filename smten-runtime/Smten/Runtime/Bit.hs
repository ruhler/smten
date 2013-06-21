
{-# LANGUAGE ScopedTypeVariables #-}

module Smten.Runtime.Bit (
    Integer, Char, S.Bit, fromInteger_Bit, show_Bit,
    ) where

import Smten.Bit
import qualified Smten.Runtime.SmtenHS as S

fromInteger_Bit :: forall n . (S.SmtenHS0 n) => Integer -> S.Bit n
fromInteger_Bit i = S.Bit $ bv_make (S.valueof0 (S.numeric :: n)) i

show_Bit :: S.Bit n -> String
show_Bit (S.Bit b) = show b
show_Bit _ = error "TODO: show_Bit with symbolic arg"

