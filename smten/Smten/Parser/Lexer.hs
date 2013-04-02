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

{-# LANGUAGE PatternGuards #-}

module Smten.Parser.Lexer (
    lexer,
    ) where

import Debug.Trace

import Prelude hiding (lex)
import Data.Char hiding (isSymbol)
import Data.Functor
import Data.Maybe (fromMaybe)

import Smten.Name
import Smten.Location
import Smten.Parser.Monad

isSmall :: Char -> Bool
isSmall '_' = True
isSmall c | isLower c = True
isSmall _ = False

isLarge :: Char -> Bool
isLarge = isUpper

isIdChar :: Char -> Bool
isIdChar c | isSmall c = True
isIdChar c | isLarge c = True
isIdChar c | isDigit c = True
isIdChar '\'' = True
isIdChar _ = False

isSymbol :: Char -> Bool
isSymbol c = c `elem` ":!#$%&*+./<=>?@\\^|-~"

singles :: [(Char, Token)]
singles = [
    ('[', TokenOpenBracket),
    (']', TokenCloseBracket),
    ('(', TokenOpenParen),
    (')', TokenCloseParen),
    ('{', TokenOpenBrace),
    ('}', TokenCloseBrace),
    (',', TokenComma),
    (';', TokenSemicolon),
    ('`', TokenBackTick)
    ]

reservedops :: [(String, Token)] 
reservedops = [
    (".", TokenPeriod),
    ("..", TokenDoubleDot),
    ("|", TokenBar),
    ("=", TokenEquals),
    (":", TokenColon),
    ("#", TokenHash),
    ("@", TokenAt),
    ("\\", TokenBackSlash),
    ("->", TokenDashArrow),
    ("<-", TokenBindArrow),
    ("=>", TokenEqualsArrow),
    ("::", TokenDoubleColon),
    ("+", TokenPlus),
    ("-", TokenMinus),
    ("*", TokenStar),
    ("$", TokenDollar),
    (">>", TokenDoubleGT),
    (">>=", TokenDoubleGTEQ),
    ("||", TokenDoubleBar),
    ("&&", TokenDoubleAmp),
    ("==", TokenDoubleEq),
    ("/=", TokenSlashEq),
    ("<", TokenLT),
    ("<=", TokenLE),
    (">=", TokenGE),
    (">", TokenGT)
    ]

keywords :: [(String, Token)]
keywords = [         
    ("data", TokenData),
    ("type", TokenType),
    ("class", TokenClass),
    ("instance", TokenInstance),
    ("where", TokenWhere),
    ("let", TokenLet),
    ("in", TokenIn),
    ("case", TokenCase),
    ("of", TokenOf),
    ("if", TokenIf),
    ("then", TokenThen),
    ("else", TokenElse),
    ("do", TokenDo),
    ("module", TokenModule),
    ("import", TokenImport),
    ("deriving", TokenDeriving)
    ]

charescs :: [(Char, Char)]
charescs = [('a', '\a'),
            ('b', '\b'),
            ('f', '\f'),
            ('n', '\n'),
            ('r', '\r'),
            ('t', '\t'),
            ('v', '\v'),
            ('\\', '\\'),
            ('"', '"'),
            ('\'', '\'')]

ischaresc :: Char -> Bool
ischaresc c = c `elem` map fst charescs

charesc :: Char -> Char
charesc c = fromMaybe (error $ "invalid charesc: " ++ [c]) (lookup c charescs)

lexstr :: String -> String -> ParserMonad Token
lexstr ostr ('"':cs) = single >> setText cs >> return (TokenString ostr)
lexstr ostr ('\\':e:cs) | ischaresc e
 = single >> single >> lexstr (ostr ++ [charesc e]) cs
lexstr ostr ('\\':cs) = error $ "todo: lex string escape: " ++ cs
lexstr ostr (c:cs) = single >> lexstr (ostr ++ [c]) cs

-- Consume the rest of a nested comment.
-- depth is the number of close "-}" to match to consume the rest of the
-- comment.
lexcomment :: Integer -> String -> ParserMonad ()
lexcomment depth ('-':'}':cs) = do
    many "-}"
    if depth == 1
        then setText cs
        else lexcomment (depth-1) cs
lexcomment depth ('{':'-':cs) = many "{-" >> lexcomment (depth+1) cs
lexcomment depth ('\n':cs) = newline >> lexcomment depth cs
lexcomment depth (c:cs) = single >> lexcomment depth cs

-- Read the next token from the input stream.
-- Updates the tloc with the location of the start of the token.
lex :: ParserMonad Token
lex = do
  let osingle t r = single >> setText r >> return t
  text <- getText
  saveLoc
  case text of
      [] -> return TokenEOF
      ('{':'-':cs) -> lexcomment 0 text >> lex
      (c:cs) | Just tok <- (lookup c singles) -> osingle tok cs
      ('\n':cs) -> newline >> setText cs >> lex
      (c:cs) | isSpace c -> single >> setText cs >> lex
      (c:cs) | isLarge c ->
          let (ns, rest) = span isIdChar cs
          in many (c:ns) >> setText rest >> return (TokenConId (name $ c:ns))
      (c:cs) | isSmall c ->
          let (ns, rest) = span isIdChar cs
          in case (c:ns) of
              kw | Just tok <- lookup kw keywords ->
                  many kw >> setText rest >> return tok
              id -> many id >> setText rest >> return (TokenVarId (name id))
      ('0':x:cs) | x `elem` ['x', 'X'] ->
          let (ns, rest) = span isHexDigit cs
          in many ('0':x:ns) >> setText rest >> return (TokenInteger (read ('0':x:ns)))
      (c:cs) | isDigit c ->
          let (ns, rest) = span isDigit cs
          in many (c:ns) >> setText rest >> return (TokenInteger (read (c:ns)))
      (c:cs) | isSymbol c ->
          let (ns, rest) = span isSymbol cs
          in case (c:ns) of
              s@(_:_:_) | all (== '-') s ->
                  setText (dropWhile (/= '\n') rest) >> lex
              rop | Just tok <- lookup rop reservedops ->
                  many rop >> setText rest >> return tok
              op | head op == ':' -> many op >> setText rest >> return (TokenConSym (name op))
              op -> do
                many op
                setText rest
                return $ TokenVarSym (name op)
      ('"':cs) -> do
         single 
         tok <- lexstr "" cs
         return tok
      ('\'':'\\':c:'\'':cs) | ischaresc c -> do
         many "'\\?'"
         setText cs
         return (TokenChar (charesc c))
      ('\'':c:'\'':cs) -> do
         many "'?'"
         setText cs
         return (TokenChar c)
          
      cs -> failE $ "fail to lex: " ++ cs

debug :: (Monad m) => String -> m ()
debug msg = --trace msg
            (return ())

-- Augment the token stream from the lexer with {n} and <n> tokens for layout
-- processing as described in the Haskell 2010 report, section 10.3
prelayout :: ParserMonad Token
prelayout = do
    ploc <- getTLoc
    ebrace <- expectBrace
    tok <- lex
    cloc <- getTLoc
    debug $ "LEX: " ++ show (tok, ploc, cloc)
    setExpectBrace (tok `elem` [TokenLet, TokenWhere, TokenDo, TokenOf])
    case (ebrace, tok) of
        (True, TokenEOF) -> return (TokenLayoutBrace 0)
        (True, t) | not (t == TokenOpenBrace
                         || (t == TokenModule && line ploc == 0)) -> do
            tpush t
            return (TokenLayoutBrace (column cloc))
        (_, t) | line cloc > line ploc -> do
            tpush t
            return (TokenLayoutLine (column cloc))
        (_, t) -> return t
    

-- Get the next token to read.
-- Takes from the token buffer first, then from the prelayout token stream.
token :: ParserMonad Token
token = do
    tbufnext <- tnext    
    case tbufnext of
        Just t -> return t
        Nothing -> prelayout

-- Perform layout processing as described in the Haskell 2010 report,
-- section 10.3
layout :: ParserMonad Token
layout = do
  tok <- token
  debug $ "PRELAYOUT: " ++ show tok
  top <- ltop
  case (tok, top) of
    (TokenLayoutLine n, Just m) | m == n -> do
        return TokenSemicolon
    (TokenLayoutLine n, Just m) | n < m -> do
        tpush tok
        lpop
        return TokenCloseBrace
    (TokenLayoutLine n, _) -> layout
    (TokenLayoutBrace n, Just m) | n > m -> do
        lpush n
        return TokenOpenBrace
    (TokenLayoutBrace n, Nothing) | n > 0 -> do
        lpush n
        return TokenOpenBrace
    (TokenLayoutBrace n, _) -> do
        -- Note: we push 0 so that when the close brace is processed by layout
        -- it goes through directly.
        lpush 0
        tpush $ TokenLayoutLine n
        tpush TokenCloseBrace
        return TokenOpenBrace
    (TokenCloseBrace, Just 0) -> do
        lpop
        return TokenCloseBrace
    (TokenCloseBrace, _) -> do
        lfailE "Parser error at '}' in layout processing"
    (TokenOpenBrace, _) -> do
        lpush 0
        return TokenOpenBrace
    -- Note: the parser inserts close brace on parser-error when needed.
    (TokenEOF, Nothing) -> return TokenEOF
    (TokenEOF, Just m) | m /= 0 -> do
        lpop
        return TokenCloseBrace
    (t, _) -> return t
        

lexer :: (Token -> ParserMonad a) -> ParserMonad a
lexer output = do
    t <- layout
    debug $ "POSTLAYOUT: " ++ show t
    output t

