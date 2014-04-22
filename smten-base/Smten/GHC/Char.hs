
module Smten.GHC.Char (chr) where

-- Note: this module is hardwired in the smten plugin to generate code to
-- Smten.Compiled.GHC.Char instead of Smten.Compiled.Smten.GHC.Char

import GHC.Base

-- TODO: Bounds check this like GHC.Char does.
chr :: Int -> Char
chr = unsafeChr

