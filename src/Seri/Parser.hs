
module Seri.Parser (
    seriparse
    ) where

import Data.Char(ord)
import Text.Parsec hiding (token)
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
    e <- (eth <|> elam <|> eparen <|> einteger <|>
          eif <|> try etrue <|> try efalse <|> try evar)
    many space
    return e

appls :: Parser Exp
appls = atom `chainl1` eapp

mults :: Parser Exp
mults = appls `chainl1` emul

adds :: Parser Exp
adds = mults `chainl1` (eadd <|> esub)

lts :: Parser Exp
lts = adds `chainl1` elt

expression :: Parser Exp
expression = lts

-- top level parser, skips initial whitespace, force match at eof.
top :: Parser Exp
top = do
    many space
    x <- expression
    eof
    return x

-- A string token followed by optional space.
token :: String -> Parser String
token x = do
    string x
    many space
    return x

eparen :: Parser Exp
eparen = do
    token "("
    x <- expression
    token ")"
    return x

etrue :: Parser Exp
etrue = do
    token "true"
    return $ BoolE True

efalse :: Parser Exp
efalse = do
    token "false"
    return $ BoolE False

eif :: Parser Exp
eif = do
    token "if"
    p <- expression
    token "then"
    tb <- expression
    token "else"
    tf <- expression
    return $ IfE UnknownT p tb tf

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
    token "+"
    return $ AddE

esub :: Parser (Exp -> Exp -> Exp)
esub = do
    token "-"
    return $ SubE

elt :: Parser (Exp -> Exp -> Exp)
elt = do
    token "<"
    return $ LtE

emul :: Parser (Exp -> Exp -> Exp)
emul = do
    token "*"
    return $ MulE

eapp :: Parser (Exp -> Exp -> Exp)
eapp = do
    return $ AppE UnknownT

elam :: Parser Exp
elam = do
    char '\\'
    nm <- name
    many space
    token "->"
    body <- expression
    return $ LamE UnknownT nm body


reserved = ["if", "then", "else", "true", "false"]

evar :: Parser Exp
evar = do
    nm <- name
    if (nm `elem` reserved)
     then fail ""
     else do
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
    
