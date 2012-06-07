
module Seri.Lambda.Parser.Utils (
    Parser,
    run, token, vname, cname, tvname,
    integer, braces,
    ) where

import Data.Char(ord)

import Text.Parsec hiding (token)
import Seri.Lambda.IR

runToEnd :: Parser a -> Parser a
runToEnd p = do 
    x <- p
    eof
    return x

-- Run a parser
-- Fails if there is a parse error.
run :: (Monad m) => Parser a -> String -> m a
run p str 
  = case (runParser (many space >> runToEnd p) () str str) of
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
symbol = oneOf "!#$%&*+./<=>@^-~"

-- A variable name
vname :: Parser Name
vname =
  let vsym = do
        s <- symbol
        ss <- many (symbol <|> char ':')
        many space
        return (s:ss)
  in vsym <|> tvname

keyword :: Parser Name 
keyword = token "case" 
            <|> token "of"
            <|> token "where"
            <|> token "value"
            <|> token "data"
            <|> token "class"
            <|> token "instance"
            <?> "keyword"

-- A type variable name
tvname :: Parser Name
tvname = do
    notFollowedBy keyword
    x <- (lower <|> char '_')
    xs <- many (alphaNum <|> char '_' <|> char '\'')
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

      list = token "[]" <|> token ":"
  in tuple <|> list <|> normal

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

