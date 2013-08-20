
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.System.IO0 (
    P.IO, return_io, bind_io, putChar, readFile,
  ) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Runtime.SymbolicOf

return_io :: a -> P.IO a
return_io = {-# SCC "PRIM_RETURN_IO" #-} P.return

bind_io :: P.IO a -> (a -> P.IO b) -> P.IO b
bind_io = {-# SCC "PRIM_BIND_IO" #-} (P.>>=)

putChar :: Char -> P.IO Unit__
putChar c = {-# SCC "PRIM_PUT_CHAR" #-} P.putChar (toHSChar c) P.>> P.return Unit__

readFile :: List__ Char -> P.IO (List__ Char)
readFile = {-# SCC "PRIM_READ_FILE" #-} symapp (\x -> do
    txt <- P.readFile x
    P.return (tosym txt))


