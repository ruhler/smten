
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.GHC.CString (unpackCString#) where

import qualified Prelude as P

import GHC.Prim
import qualified GHC.CString as GHC
import Smten.Compiled.Smten.Smten.Base

unpackCString# :: Addr# -> List__ Char
unpackCString# x = fromHSString (GHC.unpackCString# x)

