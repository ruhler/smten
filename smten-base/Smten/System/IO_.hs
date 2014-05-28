
{-# LANGUAGE NoImplicitPrelude #-}

module Smten.System.IO_ (
    IO, FilePath, putChar, putStr, putStrLn, readFile, getContents,
    print,
 ) where

import Prelude (FilePath, String, Show(..))

import Smten.Control.Monad
import Smten.System.IO0

putStr :: String -> IO ()
putStr s = mapM_ putChar s

putStrLn :: String -> IO ()
putStrLn s = do putStr s
                putStr "\n"

print :: Show a => a -> IO ()
print x = putStrLn (show x)

