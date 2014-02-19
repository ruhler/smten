
{-# LANGUAGE NoImplicitPrelude #-}

module Smten.System.IO (
    IO, FilePath,
    putChar, putStr, putStrLn, readFile, getContents,
 ) where

import Prelude (FilePath, putChar)

import Smten.Smten.Base
import Smten.Control.Monad
import Smten.System.IO0 hiding (putChar)

putStr :: String -> IO ()
putStr s = mapM_ putChar s

putStrLn :: String -> IO ()
putStrLn s = do putStr s
                putStr "\n"

