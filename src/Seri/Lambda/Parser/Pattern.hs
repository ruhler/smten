
module Seri.Lambda.Parser.Pattern (patP) where

import Text.Parsec hiding (token)

import Seri.Lambda.IR
import Seri.Lambda.Parser.Type
import Seri.Lambda.Parser.Utils

patP :: Parser Pat
patP = (try conP) <|> try wildP <|> varP <|> atomP <?> "pattern"

atomP :: Parser Pat
atomP = try parenP
       <|> aconP
       <|> integerP
       <?> "atomic pattern"

parenP :: Parser Pat
parenP = do
    token "("
    p <- patP
    token ")"
    return p

wildP :: Parser Pat
wildP = do
    token "_"
    token "::"
    t <- typeT
    return (WildP t)

conP :: Parser Pat
conP = do
    token "("
    n <- cname
    token "::"
    t <- typeT
    token ")"
    ps <- many atomP
    return (ConP (Sig n t) ps)
    
-- An atomic constructor
aconP :: Parser Pat
aconP = do
    token "("
    n <- cname
    token "::"
    t <- typeT
    token ")"
    return (ConP (Sig n t) [])

varP :: Parser Pat
varP = do
    n <- tvname
    token "::"
    t <- typeT
    return (VarP (Sig n t))

integerP :: Parser Pat
integerP = do
    x <- integer
    many space
    return (IntegerP x)
