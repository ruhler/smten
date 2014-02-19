
{-# LANGUAGE NoImplicitPrelude #-}

module Smten.System.IO (
    IO, FilePath,
    putChar, putStr, putStrLn, readFile, getContents,
 ) where

import Prelude (FilePath, putChar, getContents)

import Smten.Smten.Base
import Smten.Control.Monad
import Smten.System.IO0 hiding (putChar, getContents)

putStr :: String -> IO ()
putStr s = mapM_ putChar s

putStrLn :: String -> IO ()
putStrLn s = do putStr s
                putStr "\n"

