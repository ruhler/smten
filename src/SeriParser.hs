
module SeriParser (
    seriparse
    ) where

import Data.Char(ord)
import Text.Parsec

import Seri

-- Parse a seri expression.
-- Fails if there is a parse error.
seriparse :: (Monad m) => String -> m Exp
seriparse str
  = case (runParser expression () "???" str) of
        Left err -> fail $ show (errorPos err) ++ ": " ++ show err
        Right x -> return x

type Parser = Parsec String ()

atom :: Parser Exp
atom = eparen <|> einteger <|> evar

appls :: Parser Exp
appls = atom `chainl1` eapp

mults :: Parser Exp
mults = appls `chainl1` emul

adds :: Parser Exp
adds = mults `chainl1` eadd

expression :: Parser Exp
expression = elam <|> adds

eparen :: Parser Exp
eparen = do
    char '('
    x <- expression
    char ')'
    return x

einteger :: Parser Exp
einteger = do
    x <- integer
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
    return $ AddE

emul :: Parser (Exp -> Exp -> Exp)
emul = do
    char '*'
    return $ MulE

eapp :: Parser (Exp -> Exp -> Exp)
eapp = do
    char ' '
    return $ AppE UnknownT

elam :: Parser Exp
elam = do
    char '\\'
    nm <- name
    many1 space
    string "->"
    many1 space
    body <- expression
    return $ LamE UnknownT nm body

evar :: Parser Exp
evar = do
    nm <- name
    return $ VarE UnknownT nm

name :: Parser Name
name = many1 alphaNum

