

module Seri.Lambda.Parser.Declaration (decD) where

import Text.Parsec hiding (token)

import Seri.Lambda.IR
import Seri.Lambda.Parser.Expression
import Seri.Lambda.Parser.Pattern
import Seri.Lambda.Parser.Type
import Seri.Lambda.Parser.Utils

decD :: Parser Dec
decD = valD <|> dataD <|> classD <|> instD

valD :: Parser Dec
valD = do
    n <- vname
    char '%'
    t <- braces typeT
    token "="
    e <- expE
    return (ValD (Sig n t) e)

dataD :: Parser Dec
dataD = do
    token "data"
    n <- cname
    vs <- many tvname
    cs <- consD
    return (DataD n vs cs)

consD :: Parser [Con]
consD = do
    c <- conD 
    cs <- many (token "|" >> conD)
    return (c:cs)

conD :: Parser Con
conD = do
    n <- cname 
    ts <- many typeT
    return (Con n ts)
    
classD :: Parser Dec
classD = do
    token "class"
    n <- cname
    vs <- many vname
    token "where"
    s <- many sigD
    return (ClassD n vs s)

sigD :: Parser Sig
sigD = do
    n <- vname
    char '#'
    t <- braces typeT
    return (Sig n t)

instD :: Parser Dec
instD = do
    token "instance"
    n <- cname
    ts <- many typeT
    token "where"
    ms <- many methodD
    return (InstD n ts ms)

methodD :: Parser Method
methodD = do
    n <- vname
    token "="
    b <- expE
    return (Method n b)

