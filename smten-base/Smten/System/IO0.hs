
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
return_io = P.return

{-# NOINLINE bind_io #-}
bind_io :: IO a -> (a -> IO b) -> IO b
bind_io = (P.>>=)

{-# NOINLINE putChar #-}
putChar :: Char -> IO ()
putChar = P.putChar

{-# NOINLINE readFile #-}
readFile :: String -> IO String
readFile = P.readFile

