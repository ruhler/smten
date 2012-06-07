
{
-- vim: ft=haskell
import Data.Char

import Seri.Lambda
import Seri.Utils.Ppr

}

%name seri_decls
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
       ';'      { TokenSemicolon }
       '.'      { TokenPeriod }
       '|'      { TokenBar }
       '='      { TokenEquals }
       conid    { TokenConId $$ }
       varid    { TokenVarId $$ }
       data     { TokenData }
       forall   { TokenForall }

%%

body : topdecls             { $1 }

topdecls : topdecl                  { [$1] }
         | topdecls ';' topdecl     { $1 ++ [$3] }

topdecl :  data tycon tyvars '=' constrs      { DataD $2 $3 $5 }

type : btype               { $1 } 
     | btype '->' type     { AppT (AppT (ConT "->") $1) $3 }
     | forallty            { $1 }

btype : atype              { $1 }
      | btype atype        { AppT $1 $2 }

atype : gtycon             { ConT $1 }
      | tyvar              { VarT $1 }
      | '[' type ']'       { AppT (ConT "[]") $2 }
      | '(' types_commasep ')'
         { foldl AppT (ConT $ "(" ++ replicate (length $2 - 1) ',' ++ ")") $2 }
      | '(' type ')'       { $2 }

gtycon : tycon             { $1 }
       | '(' ')'           { "()" }
       | '[' ']'           { "[]" }
       | '(' commas ')' { "(" ++ $2 ++ ")" }
       | '(' '->' ')'      { "->" }

context :  class                        { [$1] }
        |  '(' classes_commasep ')'     { $2 }


class : qtycls tyvar               { Pred $1 [VarT $2] }

constrs : constr            { [$1] }
        | constrs constr    { $1 ++ [$2] }

constr : con atypes         { Con $1 $2 }

con : conid  { $1 }


tycon : conid              { $1 }
tyvar : varid              { $1 }
tycls : conid   { $1 }
qtycls : tycls  { $1 }



commas : ','        { "," }
       | commas ',' { ',':$1 }

types_commasep : type                       { [$1] }
               | types_commasep ',' type    { $1 ++ [$3] }

classes_commasep : class                        { [$1] }
                 | classes_commasep ',' class   { $1 ++ [$3] }

forallty : forall tyvars '.' type               { ForallT $2 [] $4 }
         | forall tyvars '.' context '=>' type  { ForallT $2 $4 $6 }

tyvars : tyvar              { [$1] }
       | tyvars tyvar       { $1 ++ [$2] }

atypes : atype              { [$1] }
       | atypes atype       { $1 ++ [$2] }

        


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
     | TokenSemicolon
     | TokenPeriod
     | TokenBar
     | TokenEquals
     | TokenConId String
     | TokenVarId String
     | TokenData
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
lexer (';':cs) = TokenSemicolon : lexer cs
lexer ('.':cs) = TokenPeriod : lexer cs
lexer ('|':cs) = TokenBar : lexer cs
lexer ('=':cs) = TokenEquals : lexer cs
lexer (c:cs) | isSpace c = lexer cs
lexer (c:cs) | isLarge c
    = let (ns, rest) = span isIdChar cs
      in TokenConId (c:ns) : lexer rest
lexer (c:cs) | isSmall c
    = let (ns, rest) = span isIdChar cs
      in case (c:ns) of
           "data" -> TokenData : lexer rest
           "forall" -> TokenForall : lexer rest
           id -> TokenVarId id : lexer rest

lexer cs = error $ "fail to lex: " ++ cs

main :: IO ()
main = do
    text <- getContents
    let lexed = lexer text
    let parsed = seri_decls lexed
    putStrLn $ render (ppr parsed)
}

