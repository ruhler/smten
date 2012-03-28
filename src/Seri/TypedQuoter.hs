
{-# LANGUAGE TemplateHaskell #-}

module Seri.TypedQuoter (
    st
    ) where

import Data.Char(ord)
import Text.Parsec hiding (token)
import Language.Haskell.Meta.Parse(parseExp)

import Language.Haskell.TH.Quote
import Language.Haskell.TH

import qualified Seri.IR as SIR
import qualified Seri.Typed as S

-- Parse a seri expression.
-- Fails if there is a parse error.
seriparse :: String -> Q Exp
seriparse str
  = case (runParser top () "???" str) of
        Left err -> fail $ show (errorPos err) ++ ": " ++ show err
        Right x -> return x

type Parser = Parsec String ()

apply :: Name -> [Exp] -> Exp
apply n exps = foldl AppE (VarE n) exps

atom :: Parser Exp
atom = do
    e <- (eth <|> elam <|> efix <|> eparen <|> einteger <|>
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
    return $ apply 'S.boolE [ConE 'True]

efalse :: Parser Exp
efalse = do
    token "false"
    return $ apply 'S.boolE [ConE 'False]

eif :: Parser Exp
eif = do
    token "if"
    p <- expression
    token "then"
    tb <- expression
    token "else"
    tf <- expression
    return $ apply 'S.ifE [p, tb, tf]

einteger :: Parser Exp
einteger = do
    x <- integer
    many space
    return $ apply 'S.integerE [LitE (IntegerL x)]

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
    return $ (\a b -> apply 'S.addE [a, b])

esub :: Parser (Exp -> Exp -> Exp)
esub = do
    token "-"
    return $ (\a b -> apply 'S.subE [a, b])

elt :: Parser (Exp -> Exp -> Exp)
elt = do
    token "<"
    return $ (\a b -> apply 'S.ltE [a, b])

emul :: Parser (Exp -> Exp -> Exp)
emul = do
    token "*"
    return $ (\a b -> apply 'S.mulE [a, b])

eapp :: Parser (Exp -> Exp -> Exp)
eapp = do
    return $ (\a b -> apply 'S.appE [a, b])

elam :: Parser Exp
elam = do
    char '\\'
    nm <- name
    many space
    token "->"
    body <- expression
    return $ apply 'S.lamE [LitE (StringL nm), LamE [VarP $ mkName nm] body]

efix :: Parser Exp
efix = do
    char '!'
    nm <- name
    many space
    body <- expression
    return $ apply 'S.fixE [LitE (StringL nm), LamE [VarP $ mkName nm] body]


reserved = ["if", "then", "else", "true", "false"]

evar :: Parser Exp
evar = do
    nm <- name
    if (nm `elem` reserved)
     then fail ""
     else do
        many space
        return $ VarE (mkName nm)

name :: Parser SIR.Name
name = many1 alphaNum

-- Parse a template haskell slice
-- @(...)
-- Where ... is haskell code with properly nested parentheses.
eth :: Parser Exp
eth = do
    string "@("
    str <- strtoclose 1
    case parseExp str of
        Right x -> return $ x
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
    

st :: QuasiQuoter 
st = QuasiQuoter qexp qpat qtype qdec

qexp :: String -> Q Exp
qexp = seriparse

qpat :: String -> Q Pat
qpat = error $ "Seri pattern quasi-quote not supported"

qtype :: String -> Q Type
qtype = error $ "Seri type quasi-quote not supported"

qdec :: String -> Q [Dec]
qdec = error $ "Seri dec quasi-quote not supported"


