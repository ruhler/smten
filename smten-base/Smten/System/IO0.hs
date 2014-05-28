
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.System.IO0 (
    IO, putChar, readFile, getContents,
    linebuffer,
 ) where

import Prelude (IO, Char, String)
import qualified Prelude as P
import Smten.Plugin.Annotations
import qualified System.IO as P

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

{-# NOINLINE linebuffer #-}
linebuffer :: IO ()
linebuffer = P.hSetBuffering P.stdout P.LineBuffering

