
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.System.IO0 (
    P.IO, putChar, readFile, getContents,
  ) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Runtime.SymbolicOf

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
