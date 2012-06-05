
module Seri.Lambda.Parser.Utils (
    Parser,
    run, token, vname, cname, tvname,
    integer, braces,
    ) where

import Data.Char(ord)

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

symbol :: Parser Char
symbol = oneOf "!#$%&*+./<=>?@^-~"

-- A variable name
vname :: Parser Name
vname =
  let normal = do
        x <- lower
        xs <- many alphaNum
        many space
        return (x:xs)

      vsym = do
        s <- symbol
        ss <- many (symbol <|> char ':')
        return (s:ss)
  in vsym <|> normal

-- A type variable name
tvname :: Parser Name
tvname = do
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

integer :: Parser Integer
integer =
    let digitstoint :: Integer -> [Char] -> Integer
        digitstoint acc [] = acc
        digitstoint acc (x:xs)  
            = digitstoint (acc*10 + (fromIntegral $ (ord x - ord '0'))) xs
    in do
        digits <- many1 digit
        many space
        return $ digitstoint 0 digits

braces :: Parser a -> Parser a
braces p = do
    token "{"
    x <- p
    token "}"
    return x

