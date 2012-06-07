
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
       '->'     { TokenDashArrow }
       '=>'     { TokenEqualsArrow }
       ','      { TokenComma }
       '.'      { TokenPeriod }
       conid    { TokenConId $$ }
       varid    { TokenVarId $$ }
       forall   { TokenForall }

%%

type : btype               { $1 } 
     | btype '->' type     { AppT (AppT (ConT "->") $1) $3 }
     | forallty            { $1 }

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


context :  class            { [$1] }
        |  context_head ')' { $1 }

context_head : '(' class                { [$2] }
             | context_head ',' class   { $1 ++ [$3] }

class : qtycls tyvar               { Pred $1 [VarT $2] }

qtycls : tycls  { $1 }

tycls : conid   { $1 }

forallty : forall tyvars '.' type               { ForallT $2 [] $4 }
         | forall tyvars '.' context '=>' type  { ForallT $2 $4 $6 }

tyvars : tyvar              { [$1] }
       | tyvars tyvar       { $1 ++ [$2] }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token = 
       TokenOpenBracket
     | TokenCloseBracket
     | TokenOpenParen
     | TokenCloseParen
     | TokenDashArrow
     | TokenEqualsArrow
     | TokenComma
     | TokenPeriod
     | TokenConId String
     | TokenVarId String
     | TokenForall
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
lexer ('-':'>':cs) = TokenDashArrow : lexer cs
lexer ('=':'>':cs) = TokenEqualsArrow : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('.':cs) = TokenPeriod : lexer cs
lexer (c:cs) | isSpace c = lexer cs
lexer (c:cs) | isLarge c
    = let (ns, rest) = span isIdChar cs
      in TokenConId (c:ns) : lexer rest
lexer (c:cs) | isSmall c
    = let (ns, rest) = span isIdChar cs
      in case (c:ns) of
           "forall" -> TokenForall : lexer rest
           id -> TokenVarId id : lexer rest

lexer cs = error $ "fail to lex: " ++ cs

main :: IO ()
main = do
    text <- getContents
    let lexed = lexer text
    let parsed = seri_type lexed
    putStrLn $ render (ppr parsed)
}

