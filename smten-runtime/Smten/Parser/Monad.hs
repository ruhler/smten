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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Parser monad used in smten parsing.
module Smten.Parser.Monad (
    Token(..), ParserMonad, runParser,
    failE, pfailE, withloc, withlocM,
    single, many, newline, getText, setText, getLoc, getTLoc, saveLoc,
    expectBrace, setExpectBrace,
    lpush, ltop, lpop, lcloseerr, tpush, tnext,
    ) where

import Control.Monad.State

import Data.Functor((<$>))

import Smten.Failable
import Smten.Name
import Smten.Location

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
     | TokenAt
     | TokenBackTick
     | TokenTilde
     | TokenBackSlash
     | TokenDoubleColon
     | TokenDoubleDot
     | TokenConId Name
     | TokenVarId Name
     | TokenVarSym Name
     | TokenConSym Name
     | TokenQConId Name
     | TokenQVarId Name
     | TokenInteger Integer
     | TokenString String
     | TokenChar Char
     | TokenData
     | TokenType
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
     | TokenForeign
     | TokenHs
     | TokenQualified
     | TokenAs
     | TokenHiding
     | TokenDeriving
     | TokenEOF
     | TokenLayoutBrace Integer
     | TokenLayoutLine Integer
     | TokenPlus
     | TokenMinus
     | TokenStar
     | TokenDollar
     | TokenDoubleGT
     | TokenDoubleGTEQ
     | TokenDoubleBar
     | TokenDoubleAmp
     | TokenDoubleEq
     | TokenSlashEq
     | TokenLT
     | TokenLE
     | TokenGE
     | TokenGT
     | TokenOpenPragma
     | TokenClosePragma
     | TokenAsInHaskell
    deriving (Eq, Show)

data PS = PS {
    ps_text :: String,          -- ^ remaining text to be parsed
    ps_loc :: Location,         -- ^ current location
    ps_tloc :: Location,        -- ^ location of the start of the previous token
    ps_lstack :: [Integer],     -- ^ The layout stack
    ps_tbuffer :: [Token],      -- ^ The token buffer
    ps_ebrace :: Bool           -- ^ True if we expect a brace next
}

type ParserMonad = StateT PS Failable

instance MonadErrorSL ParserMonad where
    errloc = gets ps_loc

-- | Run a parser given the name and text of the file to parse.
runParser :: ParserMonad a -> FilePath -> String -> Failable a
runParser p fp text = do
    (m, _) <- runStateT p (PS text (Location fp 1 1) (Location fp 0 1) [] [] True)
    return m

-- | Fail with a message.
failE :: String -> ParserMonad a
failE = lift . throw

-- | advance a single column
single :: ParserMonad ()
single = do
    col <- gets (column . ps_loc)
    modify $ \ps -> ps {
        ps_loc = (ps_loc ps) { column = 1 + col }
    }

-- | Advance a single column for each character in the given string.
many :: String -> ParserMonad ()
many = mapM_ (const single)

-- | Advance to the next line
newline :: ParserMonad ()
newline = do
    loc <- gets ps_loc
    modify $ \ps -> ps {
        ps_loc = loc {
            line = 1 + line loc,
            column = 1
        }
    }

-- | get the remaining text to parse.
getText :: ParserMonad String
getText = gets ps_text

-- | set the remaining text to parse.
setText :: String -> ParserMonad ()
setText txt = modify $ \ps -> ps { ps_text = txt }

-- | Get the current location
getLoc :: ParserMonad Location
getLoc = gets ps_loc

-- | Get the start of the previous token location
getTLoc :: ParserMonad Location
getTLoc = gets ps_tloc

-- | Save the current location as the state of the previous token location.
saveLoc :: ParserMonad ()
saveLoc = modify $ \ps -> ps { ps_tloc = ps_loc ps }

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

-- | Called by the parser when an error is encountered where a close brace is
-- expected.
lcloseerr :: ParserMonad ()
lcloseerr = do
  top <- ltop
  case top of
    Just v | v /= 0 -> lpop
    _ -> lthrow "explicit '{' can't be closed implicitly"

-- | Push a token onto the front of the token buffer, to be returned before
-- any further lexical analysis of the input text.
tpush :: Token -> ParserMonad ()
tpush t = modify $ \ps -> ps { ps_tbuffer = t : ps_tbuffer ps }

-- Take the next token, if any, from the token buffer.
tnext :: ParserMonad (Maybe Token)
tnext = do
    buf <- gets ps_tbuffer
    case buf of
        [] -> return Nothing
        x:xs -> do
            modify $ \ps -> ps { ps_tbuffer = xs }
            return $ Just x

-- | Return True if an open brace is expected as the next token.
expectBrace :: ParserMonad Bool
expectBrace = gets ps_ebrace

-- | Indicate whether we expect an open brace as the next token.
setExpectBrace :: Bool -> ParserMonad ()
setExpectBrace b = modify $ \ps -> ps { ps_ebrace = b }

pfailE :: Token -> ParserMonad a
pfailE tok = lthrow $ "parser error at " ++ show tok

withloc :: (Location -> a) -> ParserMonad a
withloc f = f <$> getLoc

withlocM :: (Location -> ParserMonad a) -> ParserMonad a
withlocM f = getLoc >>= f

