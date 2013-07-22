
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.GHC.CString (unpackCString#) where

import GHC.Prim
import qualified GHC.CString as GHC
import Smten.Compiled.Smten.Smten.Base

unpackCString# :: Addr# -> List__ Char
unpackCString# x = toList__ (GHC.unpackCString# x)

