
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.System.IO (
    IO, putChar, putStr, putStrLn,
 ) where

import qualified Prelude as P

import Smten.Smten.Base
import Smten.Control.Monad
import Smten.System.IO0

instance Monad IO where
    return = return_io
    (>>=) = bind_io
    
putStr :: String -> IO ()
putStr s = mapM_ putChar s

putStrLn :: String -> IO ()
putStrLn s = do putStr s
                putStr "\n"

