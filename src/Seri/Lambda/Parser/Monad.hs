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

-- | Parser monad used in seri parsing.
module Seri.Lambda.Parser.Monad (
    ParserMonad, runParser,
    failE, lfailE, 
    single, many, newline, getText, setText,
    ) where

import Control.Monad.State

import Seri.Failable

data PS = PS {
    ps_text :: String,          -- ^ remaining text to be parsed
    ps_line :: Integer,         -- ^ current line number
    ps_column :: Integer,       -- ^ current column number
    ps_filename :: FilePath     -- ^ name of file being parsed
}

type ParserMonad = StateT PS Failable

-- | Run a parser given the name and text of the file to parse.
runParser :: ParserMonad a -> FilePath -> String -> Failable a
runParser p fp text = do
    (m, _) <- runStateT p (PS text 1 0 fp)
    return m

-- | Fail with a message.
failE :: String -> ParserMonad a
failE = lift . fail

-- | Fail with a message augmented with location information.
lfailE :: String -> ParserMonad a
lfailE msg = do
    ln <- gets ps_line
    cl <- gets ps_column
    fp <- gets ps_filename
    failE $ fp ++ ":" ++ show ln ++ ":" ++ show cl ++ ": " ++ msg

-- | advance a single column
single :: ParserMonad ()
single = modify $ \ps -> ps {ps_column = 1 + ps_column ps }

-- | Advance a single column for each character in the given string.
many :: String -> ParserMonad ()
many = mapM_ (const single)

-- | Advance to the next line
newline :: ParserMonad ()
newline = modify $ \ps -> ps {ps_line = 1 + ps_line ps, ps_column = 0 }

-- | get the remaining text to parse.
getText :: ParserMonad String
getText = gets ps_text

-- | set the remaining text to parse.
setText :: String -> ParserMonad ()
setText txt = modify $ \ps -> ps { ps_text = txt }


