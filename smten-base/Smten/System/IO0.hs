
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.System.IO0 (
    IO, putChar, return_io, bind_io, readFile,
 ) where

import Prelude (IO)
import qualified Prelude as P
import Smten.Smten.Base
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

{-# NOINLINE return_io #-}
return_io :: a -> IO a
return_io = {-# SCC "PRIM_RETURN_IO" #-} P.return

{-# NOINLINE bind_io #-}
bind_io :: IO a -> (a -> IO b) -> IO b
bind_io = {-# SCC "PRIM_BIND_IO" #-} (P.>>=)

{-# NOINLINE putChar #-}
putChar :: Char -> IO ()
putChar = {-# SCC "PRIM_PUT_CHAR" #-} P.putChar

{-# NOINLINE readFile #-}
readFile :: String -> IO String
readFile = {-# SCC "PRIM_READ_FILE" #-} P.readFile

