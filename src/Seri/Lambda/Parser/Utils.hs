
module Seri.Lambda.Parser.Utils (
    Parser,
    run, token, vname, cname,
    ) where

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

