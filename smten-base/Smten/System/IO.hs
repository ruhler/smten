
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.System.IO (
    IO, putChar, putStr, putStrLn,
 ) where

import qualified Prelude as P

import Smten.Smten.Base
import Smten.Control.Monad

type IO = P.IO

instance Monad IO where
    return = P.return
    (>>=) = (P.>>=)
    
putChar :: Char -> IO ()
putChar = P.putChar

putStr :: String -> IO ()
putStr s = mapM_ putChar s

putStrLn :: String -> IO ()
putStrLn s = do putStr s
                putStr "\n"

