
module Smten.Base.GHC.Char (chr) where

import GHC.Base

-- TODO: Bounds check this like GHC.Char does.
chr :: Int -> Char
chr = unsafeChr

