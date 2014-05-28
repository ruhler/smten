
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.System.IO0 (
    P.IO, putChar, readFile, getContents, linebuffer,
  ) where

import qualified Prelude as P
import qualified System.IO as P
import Smten.Runtime.SymbolicOf
import Smten.Compiled.Smten.Smten.Base

putChar :: Char -> P.IO Unit__
putChar c = {-# SCC "PRIM_PUT_CHAR" #-} P.putChar (toHSChar c) P.>> P.return __Unit__

readFile :: List__ Char -> P.IO (List__ Char)
readFile = {-# SCC "PRIM_READ_FILE" #-} symapp (\x -> do
    txt <- P.readFile x
    P.return (tosym txt))


getContents :: P.IO (List__ Char)
getContents = {-# SCC "PRIM_GET_CONTENTS" #-} do
    txt <- P.getContents
    P.return (tosym txt)

linebuffer :: P.IO Unit__
linebuffer = P.hSetBuffering P.stdout P.LineBuffering P.>> P.return __Unit__

