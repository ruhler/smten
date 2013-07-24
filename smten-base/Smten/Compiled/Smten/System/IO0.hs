
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.System.IO0 (
    P.IO, return_io, bind_io, putChar,
  ) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base

return_io :: a -> P.IO a
return_io = P.return

bind_io :: P.IO a -> (a -> P.IO b) -> P.IO b
bind_io = (P.>>=)

putChar :: Char -> P.IO Unit__
putChar c = P.putChar (toHSChar c) P.>> P.return Unit__

