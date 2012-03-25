
module Seri.Parser (
    seriparse
    ) where

import Data.Char(ord)
import Text.Parsec
import Language.Haskell.Meta.Parse(parseExp)

import Seri.IR

-- Parse a seri expression.
-- Fails if there is a parse error.
seriparse :: (Monad m) => String -> m Exp
seriparse str
  = case (runParser top () "???" str) of
        Left err -> fail $ show (errorPos err) ++ ": " ++ show err
        Right x -> return x

type Parser = Parsec String ()

atom :: Parser Exp
atom = do
    e <- (eth <|> elam <|> eparen <|> einteger <|> etrue <|> efalse <|> evar)
    many space
    return e

appls :: Parser Exp
appls = atom `chainl1` eapp

mults :: Parser Exp
mults = appls `chainl1` emul

adds :: Parser Exp
adds = mults `chainl1` (eadd <|> esub)

expression :: Parser Exp
expression = adds

-- top level parser, skips initial whitespace, force match at eof.
top :: Parser Exp
top = do
    many space
    x <- expression
    eof
    return x

eparen :: Parser Exp
eparen = do
    char '('
    many space
    x <- expression
    many space
    char ')'
    many space
    return x

etrue :: Parser Exp
etrue = do
    string "true"
    many space
    return $ BoolE True

efalse :: Parser Exp
efalse = do
    string "false"
    many space
    return $ BoolE False

einteger :: Parser Exp
einteger = do
    x <- integer
    many space
    return $ IntegerE x

integer :: Parser Integer
integer = do 
    digits <- many1 digit
    return $ digitstoint 0 digits

digitstoint :: Integer -> [Char] -> Integer
digitstoint acc [] = acc
digitstoint acc (x:xs) = digitstoint (acc*10 + (fromIntegral $ (ord x - ord '0'))) xs
    
eadd :: Parser (Exp -> Exp -> Exp)
eadd = do
    char '+'
    many space
    return $ AddE

esub :: Parser (Exp -> Exp -> Exp)
esub = do
    char '-'
    many space
    return $ SubE

emul :: Parser (Exp -> Exp -> Exp)
emul = do
    char '*'
    many space
    return $ MulE

eapp :: Parser (Exp -> Exp -> Exp)
eapp = do
    return $ AppE UnknownT

elam :: Parser Exp
elam = do
    char '\\'
    nm <- name
    many space
    string "->"
    many space
    body <- expression
    many space
    return $ LamE UnknownT nm body

evar :: Parser Exp
evar = do
    nm <- name
    many space
    return $ VarE UnknownT nm

name :: Parser Name
name = many1 alphaNum

-- Parse a template haskell slice
-- @(...)
-- Where ... is haskell code with properly nested parentheses.
eth :: Parser Exp
eth = do
    string "@("
    str <- strtoclose 1
    case parseExp str of
        Right x -> return $ ThE x
        Left err -> fail err

strtoclose :: Integer -> Parser String
strtoclose n = do
    c <- anyChar
    case (c, n) of
        (')', 1) -> return ""
        (')', _) -> do
            s <- strtoclose (n-1)
            return (c:s)
        ('(', _) -> do
            s <- strtoclose (n+1)
            return (c:s)
        (_, _) -> do
            s <- strtoclose n
            return (c:s)
    
