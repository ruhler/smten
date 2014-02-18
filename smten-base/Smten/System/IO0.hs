
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.System.IO0 (
    IO, putChar, readFile, getContents,
 ) where

import Prelude (IO)
import qualified Prelude as P
import Smten.Smten.Base
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

{-# NOINLINE putChar #-}
putChar :: Char -> IO ()
putChar = {-# SCC "PRIM_PUT_CHAR" #-} P.putChar

{-# NOINLINE readFile #-}
readFile :: String -> IO String
readFile = {-# SCC "PRIM_READ_FILE" #-} P.readFile

{-# NOINLINE getContents #-}
getContents :: IO String
getContents = {-# SCC "PRIM_GET_CONTENTS" #-} P.getContents

