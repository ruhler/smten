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

module Seri.Lambda.Parser.Lexer (
    Token(..), lexer,
    ) where

import Data.Char hiding (isSymbol)
import Data.Maybe (fromMaybe, fromJust)

import Seri.Lambda.Parser.Monad

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
     | TokenBackSlash
     | TokenDoubleColon
     | TokenConId String
     | TokenVarId String
     | TokenVarSym String
     | TokenConSym String
     | TokenInteger Integer
     | TokenString String
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
     | TokenEOF
    deriving (Show)

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
    (';', TokenSemicolon)
    ]

reservedops :: [(String, Token)] 
reservedops = [
    (".", TokenPeriod),
    ("|", TokenBar),
    ("=", TokenEquals),
    (":", TokenColon),
    ("\\", TokenBackSlash),
    ("->", TokenDashArrow),
    ("<-", TokenBindArrow),
    ("=>", TokenEqualsArrow),
    ("::", TokenDoubleColon)
    ]

keywords :: [(String, Token)]
keywords = [         
    ("data", TokenData),
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
    ("import", TokenImport)
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

lexer :: (Token -> ParserMonad a) -> ParserMonad a
lexer output = do
  let osingle t r = single >> setText r >> output t
  text <- getText
  case text of
      [] -> output TokenEOF
      (c:cs) | (c `elem` (map fst singles)) ->
          osingle (fromJust (lookup c singles)) cs
      ('\n':cs) -> newline >> setText cs >> lexer output
      (c:cs) | isSpace c -> single >> setText cs >> lexer output
      (c:cs) | isLarge c ->
          let (ns, rest) = span isIdChar cs
          in many (c:ns) >> setText rest >> (output $ TokenConId (c:ns))
      (c:cs) | isSmall c ->
          let (ns, rest) = span isIdChar cs
          in case (c:ns) of
              kw | kw `elem` (map fst keywords) ->
                  many kw >> setText rest >> output (fromJust (lookup kw keywords))
              id -> many id >> setText rest >> output (TokenVarId id)
      (c:cs) | isDigit c ->
          let (ns, rest) = span isDigit cs
          in many (c:ns) >> setText rest >> output (TokenInteger (read (c:ns)))
      (c:cs) | isSymbol c ->
          let (ns, rest) = span isSymbol cs
          in case (c:ns) of
              "--" -> setText (dropWhile (/= '\n') rest) >> lexer output
              rop | rop `elem` (map fst reservedops) ->
                  many rop >> setText rest >> output (fromJust (lookup rop reservedops))
              op | head op == ':' -> many op >> setText rest >> output (TokenConSym op)
              op -> many op >> setText rest >> output (TokenVarSym op)
      ('"':cs) -> single >> lexstr "" cs >>= output
          
      cs -> failE $ "fail to lex: " ++ cs



