
module Seri.Lambda.Parser.Type (typeT, atomT, predicateT) where

import Text.Parsec hiding (token)

import Seri.Lambda.IR
import Seri.Lambda.Parser.Utils

typeT :: Parser Type
typeT = forallT <|> appsT <?> "type"

forallT :: Parser Type
forallT = do
    token "forall"
    tvars <- many tvname
    token "."
    ctx <- option [] contextT
    t <- typeT
    return (ForallT tvars ctx t)

contextT :: Parser [Pred]
contextT = do
    token "("
    p <- predicateT
    ps <- many (token "," >> predicateT)
    token ")"
    token "=>"
    return (p:ps)

predicateT :: Parser Pred
predicateT = do
    n <- cname
    ts <- many atomT
    return (Pred n ts)

appsT :: Parser Type
appsT = atomT `chainl1` appT

appT :: Parser (Type -> Type -> Type)
appT = return AppT

atomT :: Parser Type
atomT = (try parenT) <|> (try listT) <|> (try tupT) <|> conT <|> varT <?> "atom type"

parenT :: Parser Type
parenT = do
    token "("
    x <- typeT
    token ")"
    return x

conT :: Parser Type
conT = do
    n <- (cname <|> token "->")
    return (ConT n)

varT :: Parser Type
varT = do
    n <- tvname
    return (VarT n)

listT :: Parser Type
listT = do
    token "["
    t <- typeT
    token "]"
    return (AppT (ConT "[]") t)

tupT :: Parser Type
tupT = do
    token "("
    t <- typeT
    ts <- many (token "," >> typeT)
    token ")"
    return (foldl AppT (ConT $ "(" ++ replicate (length ts) ',' ++ ")") (t:ts))

