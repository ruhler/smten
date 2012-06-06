
module Seri.Lambda.Parser.Expression (expE, matchE) where

import Text.Parsec hiding (token)

import Seri.Lambda.IR
import Seri.Lambda.Parser.Pattern
import Seri.Lambda.Parser.Type
import Seri.Lambda.Parser.Utils

expE :: Parser Exp
expE = atomE `chainl1` appE

appE :: Parser (Exp -> Exp -> Exp)
appE = return AppE

atomE :: Parser Exp
atomE = (try parenE) <|> integerE <|> (try caseE) <|> varE <|> lamE <|> (try conE) <?> "atom expression"

parenE :: Parser Exp
parenE = do
    token "(" 
    e <- expE
    token ")"
    return e

integerE :: Parser Exp
integerE = do
    x <- integer
    return (IntegerE x)

-- Variable or primitive
varE :: Parser Exp
varE = do
    kind <- oneOf ".%@#"
    n <- vname
    case kind of
        '.' -> do
            t <- braces typeT
            return (VarE (Sig n t) Bound)
        '%' -> do
            t <- braces typeT
            return (VarE (Sig n t) Declared)
        '@' -> do
            t <- braces typeT
            return (PrimE (Sig n t))
        '#' -> do
            (t, (Pred ni tis)) <- braces $ do
                t <- typeT
                token ","
                p <- predicateT
                return (t, p)
            return (VarE (Sig n t) (Instance ni tis))

conE :: Parser Exp
conE = do
    n <- cname
    t <- braces typeT
    many space
    return (ConE (Sig n t))

lamE :: Parser Exp
lamE = do
    char '\\'
    n <- tvname
    t <- braces (typeT)
    token "->"
    e <- expE
    return (LamE (Sig n t) e)

caseE :: Parser Exp
caseE = do
    token "case"
    e <- expE
    token "of"
    ms <- braces (many matchE)
    return (CaseE e ms)

matchE :: Parser Match
matchE = do
    p <- patP
    token "->"
    e <- expE
    token ";"
    return (Match p e)
                   
