
module Seri.Lambda.Parser (
    parseDecs, parseExp, parseType, parsePat
    ) where

import Text.Parsec hiding (token)

import Seri.Lambda.Parser.Declaration
import Seri.Lambda.Parser.Expression
import Seri.Lambda.Parser.Pattern
import Seri.Lambda.Parser.Type
import Seri.Lambda.Parser.Utils
import Seri.Lambda.IR

parseDecs :: (Monad m) => String -> m [Dec]
parseDecs = run (many decD)

parseExp :: (Monad m) => String -> m Exp
parseExp = run expE

parseType :: (Monad m) => String -> m Type
parseType = run typeT

parsePat :: (Monad m) => String -> m Pat
parsePat = run patP

