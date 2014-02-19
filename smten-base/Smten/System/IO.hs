
{-# LANGUAGE NoImplicitPrelude #-}

module Smten.System.IO (
    IO, FilePath,
    putChar, putStr, putStrLn, readFile, getContents,
 ) where

import Prelude (FilePath)

import Smten.Smten.Base
import Smten.Control.Monad
import Smten.System.IO0

putStr :: String -> IO ()
putStr s = mapM_ putChar s

putStrLn :: String -> IO ()
putStrLn s = do putStr s
                putStr "\n"

