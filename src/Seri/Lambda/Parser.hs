
module Seri.Lambda.Parser (tests) where

import Test.HUnit

import Text.Parsec hiding (token)
import Seri.Lambda.IR

-- Run a parser
-- Fails if there is a parse error.
run :: (Monad m) => Parser a -> String -> m a
run p str 
  = case (runParser p () str str) of
        Left err -> fail $ show err
        Right x -> return x

type Parser = Parsec String ()

typeT :: Parser Type
typeT = forallT <|> appsT

forallT :: Parser Type
forallT = do
    token "forall"
    tvars <- many vname
    token "."
    ctx <- option [] context
    t <- typeT
    return (ForallT tvars ctx t)

context :: Parser [Pred]
context = do
    token "("
    p <- predicate
    ps <- many (token "," >> predicate)
    token ")"
    token "=>"
    return (p:ps)

predicate :: Parser Pred
predicate = do
    n <- cname
    ts <- many typeT
    return (Pred n ts)

appsT :: Parser Type
appsT = atomT `chainl1` appT

appT :: Parser (Type -> Type -> Type)
appT = return AppT

atomT :: Parser Type
atomT = parenT <|> conT <|> varT

parenT :: Parser Type
parenT = do
    token "("
    x <- typeT
    token ")"
    return x

conT :: Parser Type
conT = do
    n <- cname
    return (ConT n)

varT :: Parser Type
varT = do
    n <- vname
    return (VarT n)

-- A string token followed by optional space.
token :: String -> Parser String
token x = do
    string x
    many space
    return x

-- A variable name
vname :: Parser Name
vname = do
    x <- lower
    xs <- many alphaNum
    many space
    return (x:xs)

-- A constructor name
cname :: Parser Name
cname =
  let normal = do
        x <- upper
        xs <- many alphaNum
        many space
        return (x:xs)

      tuple = do
        string "("
        cs <- many (char ',')
        token ")"
        return $ "(" ++ cs ++ ")"

      arrow = token "->"
      list = token "[]"
  in arrow <|> tuple <|> list <|> normal


tests = "Parser" ~: [
    "simple" ~:
        Right (AppT (AppT (ConT "->") (VarT "a")) (ConT "Integer"))
        ~=? (run typeT "-> a Integer" :: Either String Type),
    "forall" ~:
        Right (ForallT ["a", "m"] [ 
                Pred "Foo" [VarT "a"], Pred "Monad" [VarT "m"]]
                (AppT (AppT (ConT "->") (VarT "a")) (AppT (VarT "m") (VarT "a"))))
        ~=? (run typeT "forall a m . (Foo a, Monad m) => -> a (m a)" :: Either String Type)
    ]

