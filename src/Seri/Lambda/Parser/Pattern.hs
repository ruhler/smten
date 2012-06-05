
module Seri.Lambda.Parser.Pattern (patP) where

import Text.Parsec hiding (token)

import Seri.Lambda.IR
import Seri.Lambda.Parser.Type
import Seri.Lambda.Parser.Utils

patP :: Parser Pat
patP = atomP `chainl1` appP

appP :: Parser (Pat -> Pat -> Pat)
appP = return (AppP)

atomP :: Parser Pat
atomP = (try parenP) <|> (try wildP) <|> conP <|> varP <|> integerP <?> "atom pattern"

parenP :: Parser Pat
parenP = do
    token "("
    p <- patP
    token ")"
    return p

wildP :: Parser Pat
wildP = do
    string "_"
    t <- braces typeT
    return (WildP t)

conP :: Parser Pat
conP = do
    n <- cname
    t <- braces typeT
    return (ConP (Sig n t))
    
varP :: Parser Pat
varP = do
    n <- tvname
    t <- braces typeT
    return (VarP (Sig n t))

integerP :: Parser Pat
integerP = do
    x <- integer
    many space
    return (IntegerP x)
