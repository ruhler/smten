
{
-- vim: ft=haskell
import Data.Char

import Seri.Lambda
import Seri.Utils.Ppr

}

%name seri_type
%tokentype { Token }
%error { parseError }

%token 
       '['      { TokenOpenBracket }
       ']'      { TokenCloseBracket }
       '('      { TokenOpenParen }
       ')'      { TokenCloseParen }
       '->'     { TokenArrow }
       ','      { TokenComma }
       conid    { TokenConId $$ }
       varid    { TokenVarId $$ }

%%

type : btype               { $1 } 
     | btype '->' type     { AppT (AppT (ConT "->") $1) $3 }

btype : atype              { $1 }
      | btype atype        { AppT $1 $2 }

atype : gtycon             { ConT $1 }
      | tyvar              { VarT $1 }
      | '[' type ']'       { AppT (ConT "[]") $2 }
      | tuplety_head ')'
         { foldl AppT (ConT $ "(" ++ replicate (length $1 - 1) ',' ++ ")") $1 }
      | '(' type ')'       { $2 }

gtycon : tycon             { $1 }
       | '(' ')'           { "()" }
       | '[' ']'           { "[]" }
       | tuplecon_head ')' { $1 ++ ")" }
       | '(' '->' ')'      { "->" }

tuplecon_head : '(' ','               { "(," }
              | tuplecon_head ','     { $1 ++ "," }

tuplety_head : '(' type               { [$2] }
             | tuplety_head ',' type  { $1 ++ [$3] }

tycon : conid              { $1 }

tyvar : varid              { $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token = 
       TokenOpenBracket
     | TokenCloseBracket
     | TokenOpenParen
     | TokenCloseParen
     | TokenArrow
     | TokenComma
     | TokenConId String
     | TokenVarId String
    deriving (Show)

isSmall :: Char -> Bool
isSmall '_' = True
isSmall c | isLower c = True
isSmall _ = False

isLarge :: Char -> Bool
isLarge = isUpper

isIdChar :: Char -> Bool
isIdChar c | isSmall c = True
isIdChar c | isLarge c = True
isIdChar c | isDigit c = True
isIdChar '\'' = True
isIdChar _ = False

lexer :: String -> [Token]
lexer [] = []
lexer ('[':cs) = TokenOpenBracket : lexer cs
lexer (']':cs) = TokenCloseBracket : lexer cs
lexer ('(':cs) = TokenOpenParen : lexer cs
lexer (')':cs) = TokenCloseParen : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer (c:cs) | isSpace c = lexer cs
lexer (c:cs) | isLarge c
    = let (ns, rest) = span isIdChar cs
      in TokenConId (c:ns) : lexer rest
lexer (c:cs) | isSmall c
    = let (ns, rest) = span isIdChar cs
      in TokenVarId (c:ns) : lexer rest
lexer cs = error $ "fail to lex: " ++ cs

main :: IO ()
main = do
    text <- getContents
    let lexed = lexer text
    let parsed = seri_type lexed
    putStrLn $ render (ppr parsed)
}

