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
    Token(..), ParserMonad, runParser,
    failE, lfailE, 
    single, many, newline, getText, setText,
    lpush, ltop, lpop, tpush, tnext,
    ) where

import Control.Monad.State

import Seri.Failable

data Token = 
       TokenOpenBracket
     | TokenCloseBracket
     | TokenOpenParen
     | TokenCloseParen
     | TokenOpenBrace
     | TokenCloseBrace
     | TokenDashArrow
     | TokenBindArrow
     | TokenEqualsArrow
     | TokenComma
     | TokenSemicolon
     | TokenPeriod
     | TokenBar
     | TokenEquals
     | TokenColon
     | TokenHash
     | TokenBackSlash
     | TokenDoubleColon
     | TokenConId String
     | TokenVarId String
     | TokenVarSym String
     | TokenConSym String
     | TokenInteger Integer
     | TokenString String
     | TokenChar Char
     | TokenData
     | TokenForall
     | TokenClass
     | TokenInstance
     | TokenWhere
     | TokenLet
     | TokenIn
     | TokenCase
     | TokenOf
     | TokenIf
     | TokenThen
     | TokenElse
     | TokenDo
     | TokenModule
     | TokenImport
     | TokenDeriving
     | TokenEOF
     | TokenLayoutBrace Integer
     | TokenLayoutLine Integer
    deriving (Show)

data PS = PS {
    ps_text :: String,          -- ^ remaining text to be parsed
    ps_line :: Integer,         -- ^ current line number
    ps_column :: Integer,       -- ^ current column number
    ps_filename :: FilePath,    -- ^ name of file being parsed
    ps_lstack :: [Integer],     -- ^ The layout stack
    ps_tbuffer :: [Token]       -- ^ The token buffer
}

type ParserMonad = StateT PS Failable

-- | Run a parser given the name and text of the file to parse.
runParser :: ParserMonad a -> FilePath -> String -> Failable a
runParser p fp text = do
    (m, _) <- runStateT p (PS text 1 0 fp [] [])
    return m

-- | Fail with a message.
failE :: String -> ParserMonad a
failE = lift . throw

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



-- | Return the top of the layout stack
ltop :: ParserMonad (Maybe Integer)
ltop = do
    stack <- gets ps_lstack
    case stack of
        [] -> return Nothing
        (x:_) -> return (Just x)

-- | Push an element onto the top of the layout stack
lpush :: Integer -> ParserMonad ()
lpush n = modify $ \ps -> ps { ps_lstack = n : (ps_lstack ps) }

-- | Pop an element off the top of the layout stack.
lpop :: ParserMonad ()
lpop = modify $ \ps -> ps {
    ps_lstack = case ps_lstack ps of
                   [] -> error "lpop on empty layout stack in ParserMonad"
                   _:xs -> xs
    }

-- | Push a token onto the back of the token buffer, to be returned before
-- any further lexical analysis of the input text.
--
-- Tokens come out in the order they are tpushed (FIFO, not LIFO).
tpush :: Token -> ParserMonad ()
tpush t = modify $ \ps -> ps { ps_tbuffer = ps_tbuffer ps ++ [t] }

-- Take the next token, if any, from the token buffer.
tnext :: ParserMonad (Maybe Token)
tnext = do
    buf <- gets ps_tbuffer
    case buf of
        [] -> return Nothing
        x:xs -> do
            modify $ \ps -> ps { ps_tbuffer = xs }
            return $ Just x

