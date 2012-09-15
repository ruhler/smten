-------------------------------------------------------------------------------
-- Copyright (c) 2012      SRI International, Inc. 
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory under DARPA/AFRL contract (FA8750-10-C-0237)
-- ("CTSRD"), as part of the DARPA CRASH research programme.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
-------------------------------------------------------------------------------
--
-- Authors: 
--   Richard Uhler <ruhler@csail.mit.edu>
-- 
-------------------------------------------------------------------------------

-- | A monad for dealing with computations which can fail.
module Seri.Failable (
    Failable, throw, attempt, attemptM, attemptIO, surely, onfail,
    ) where

import Control.Monad

import System.IO
import System.Exit

type Failable = Either String

-- | Throw an error with the given error message.
throw :: String -> Failable a
throw = Left

-- | Run a Failable computation, returning either a failure message or the
-- result of the computation.
attempt :: Failable a -> Either String a
attempt = id

-- | Attempt a failable computation in a Monad.
-- fails in the monad if failable fails.
attemptM :: (Monad m) => Failable a -> m a
attemptM (Left msg) = fail msg
attemptM (Right a) = return a

-- | Attempt a failable computation in IO.
-- Prints the error message and exits failure on failure.
attemptIO :: Failable a -> IO a
attemptIO (Left msg) = do
    hPutStrLn stderr msg
    exitFailure
attemptIO (Right a) = return a

-- | Return the result of a failable computation sure to complete.
-- It's an error if the computation fails.
surely :: Failable a -> a
surely (Right a) = a
surely (Left msg) = error msg
    
-- | Run computation 'c', if it fails, return the result of calling 'f' on the
-- error message from the failing 'c'.
onfail :: (String -> Failable a) -- ^ f
       -> Failable a             -- ^ c
       -> Failable a
onfail f (Left msg) = f msg
onfail f c = c

