
{

-- vim: ft=haskell
module Seri.Lambda.Parser (parseDecs) where

import Data.Char hiding (isSymbol)
import Data.Maybe

import Control.Monad.State

import Seri.Lambda.IR
import Seri.Lambda.Sugar
import Seri.Utils.Ppr (Ppr(..), render)

}

%name seri_decls body
%tokentype { Token }
%error { parseError }
%monad { ParserMonad }
%lexer { lexer } { TokenEOF }

%token 
       '['      { TokenOpenBracket }
       ']'      { TokenCloseBracket }
       '('      { TokenOpenParen }
       ')'      { TokenCloseParen }
       '{'      { TokenOpenBrace }
       '}'      { TokenCloseBrace }
       '->'     { TokenDashArrow }
       '<-'     { TokenBindArrow }
       '=>'     { TokenEqualsArrow }
       ','      { TokenComma }
       ';'      { TokenSemicolon }
       '.'      { TokenPeriod }
       '|'      { TokenBar }
       '='      { TokenEquals }
       '@'      { TokenAtSign }
       '%'      { TokenPercent }
       '#'      { TokenHash }
       ':'      { TokenColon }
       '\\'      { TokenBackSlash }
       '::'      { TokenDoubleColon }
       conid    { TokenConId $$ }
       varid    { TokenVarId $$ }
       varsym   { TokenVarSym $$ }
       consym   { TokenConSym $$ }
       integer  { TokenInteger $$ }
       'data'   { TokenData }
       'forall' { TokenForall }
       'class'  { TokenClass }
       'instance'  { TokenInstance }
       'where'  { TokenWhere }
       'case'   { TokenCase }
       'of'     { TokenOf }
       'if'     { TokenIf }
       'then'     { TokenThen }
       'else'     { TokenElse }
       'do'     { TokenDo }
       EOF      { TokenEOF }

%%

body :: { [PDec] }
 : topdecls
    { $1 }
 | topdecls ';'
    { $1 }

topdecls :: { [PDec] }
 : topdecl
    { [$1] }
 | topdecls ';' topdecl
    { $1 ++ [$3] }

topdecl :: { PDec }
 : 'data' tycon tyvars '=' constrs
    { PDec (DataD $2 $3 $5) }
 | 'data' tycon '=' constrs
    { PDec (DataD $2 [] $4) }
 | 'class' tycls tyvars 'where' '{' cdecls '}'
    { PDec (ClassD $2 $3 $6)}
 | 'class' tycls tyvars 'where' '{' cdecls ';' '}'
    { PDec (ClassD $2 $3 $6)}
 | 'instance' class 'where' '{' idecls '}'
    { PDec (InstD $2 $5) }
 | 'instance' class 'where' '{' idecls ';' '}'
    { PDec (InstD $2 $5) }
 | decl
    { $1 }

decl :: { PDec }
 : gendecl
    { PSig $1 }
 | funlhs rhs
    { PClause (fst $1) (Clause (snd $1) $2) }

funlhs :: { (Name, [Pat]) }
 : var apats
    { ($1, $2) } 
 | var
    { ($1, []) }

rhs :: { Exp }
 : '=' exp
    { $2 }

cdecls :: { [Sig] }
 : cdecl
    { [$1] }
 | cdecls ';' cdecl
    { $1 ++ [$3] }

cdecl :: { Sig }
 : gendecl
    { $1 }

idecls :: { [Method] }
 : idecl
    { [$1] }
 | idecls ';' idecl
    { $1 ++ [$3] }

idecl :: { Method }
 : funlhs rhs
    { Method (fst $1) (clauseE [Clause (snd $1) $2]) }

gendecl :: { Sig }
 : var '::' type
    { Sig $1 $3 }

type :: { Type }
 : btype
    { $1 } 
 | btype '->' type
    { AppT (AppT (ConT "->") $1) $3 }
 | forallty
    { $1 }

btype :: { Type }
 : atype
    { $1 }
 | btype atype
    { AppT $1 $2 }

atype :: { Type }
 : gtycon
    { ConT $1 }
 | tyvar
    { VarT $1 }
 | '[' type ']'
    { AppT (ConT "[]") $2 }
 | '(' types_commasep ')'
    { foldl AppT (ConT $ "(" ++ replicate (length $2 - 1) ',' ++ ")") $2 }
 | '(' type ')'
    { $2 }

gtycon :: { String }
 : tycon
    { $1 }
 | '(' ')'
    { "()" }
 | '[' ']'
    { "[]" }
 | '(' commas ')'
    { "(" ++ $2 ++ ")" }
 | '(' '->' ')'
    { "->" }

context :: { [Class] }
 : '(' classes_commasep ')'
    { $2 }


class :: { Class }
 : qtycls atypes
    { Class $1 $2 }

constrs :: { [Con] }
 : constr
    { [$1] }
 | constrs '|' constr
    { $1 ++ [$3] }

constr :: { Con }
 : con atypes
    { Con $1 $2 }
 | con
    { Con $1 [] }

exp :: { Exp }
 : exp10
    { $1 }
 | exp10 qop exp10
    { AppE (AppE $2 $1) $3 }

exp10 :: { Exp }
 : '\\' var_typed '->' exp
    { LamE $2 $4 }
 | 'if' exp 'then' exp 'else' exp
    { ifE $2 $4 $6 }
 | 'case' exp 'of' '{' alts '}'
    { CaseE $2 $5 }
 | '#' '{' class '}' 'do' '{' stmts exp ';' '}' 
    { doE (Instance $3) ($7 ++ [NoBindS $8]) }
 | 'do' '{' stmts exp ';' '}'
    { doE UnknownVI ($3 ++ [NoBindS $4]) }
 | fexp
    { $1 }

fexp :: { Exp }
 : aexp
    { $1 }
 | fexp aexp
    { AppE $1 $2 }

aexp :: { Exp }
 : qvar_withinfo
    { $1 }
 | gcon_typed
    { ConE $1 }
 | integer
    { IntegerE $1 }
 | '(' exp ')'
    { $2 }
 | '(' exp ',' exps_commasep ')'
    { tupE ($2 : $4) }
 | '['  exps_commasep ']'
    { listE $2 }

-- TODO: Haskell doesn't allow a semicolon after the last alternative.
alts :: { [Match] }
 : alt ';'
    { [$1] }
 | alts alt ';'
    { $1 ++ [$2] }

alt :: { Match }
 : pat '->' exp
    { Match $1 $3 }

stmts :: { [Stmt] }
 : stmt 
    { [$1] }
 | stmts stmt
    { $1 ++ [$2] }

stmt :: { Stmt }
 : exp ';'
    { NoBindS $1 }
 | var_typed '<-' exp ';'
    { BindS $1 $3 }

pat :: { Pat }
 : pat10
    { $1 }
 | pat10 ':' pat
    { ConP (Sig ":" UnknownT) [$1, $3] }

pat10 :: { Pat }
 : gcon_typed apats
    { ConP $1 $2 }
 | apat
    { $1 }

apats :: { [Pat] }
 : apat
    { [$1] }
 | apats apat
    { $1 ++ [$2] }

apat :: { Pat }
 : var_typed
    { let Sig n t = $1 in if n == "_" then WildP t else VarP $1 }
 | gcon_typed
    { ConP $1 [] }
 | '(' pat ')'
    { $2 }
 | '(' pat ',' pats_commasep ')'
    { tupP ($2 : $4) }
 | integer
    { IntegerP $1 }

var_typed :: { Sig }
 : '(' var  '::' type ')'
    { Sig $2 $4 }
 | var
    { Sig $1 UnknownT }

gcon_typed :: { Sig }
 : '(' gcon '::' type ')'
    { Sig $2 $4 }
 | gcon
    { Sig $1 UnknownT }

qvar_withinfo :: { Exp }
 : '(' '.' qvar '::' type ')'
    { VarE (Sig $3 $5) Bound }
 | '(' '@' qvar '::' type ')'
    { PrimE (Sig $3 $5) }
 | '(' '%' qvar '::' type ')'
    { VarE (Sig $3 $5) Declared }
 | '(' '#' '{' class '}' qvar '::' type ')' 
    { VarE (Sig $6 $8) (Instance $4) }
 | qvar
    { VarE (Sig $1 UnknownT) UnknownVI }

gcon :: { String }
 : '(' ')'
    { "()" }
 | '[' ']'
    { "[]" }
 | '(' commas ')'
    { "(" ++ $2 ++ ")" }
 | qcon
    { $1 }

var :: { String }
 : varid
    { $1 }
 | '(' varsym ')'
    { $2 }

qvar :: { String }
 : qvarid
    { $1 }
 | '(' qvarsym ')'
    { $2 }

qop :: { Exp }
 : qvarsym
    { VarE (Sig $1 UnknownT) UnknownVI }
 | qconop
    { ConE (Sig $1 UnknownT) }

qvarsym :: { String }
 : varsym
    { $1 }

con :: { String }
 : conid
    { $1 }

qcon :: { String }
 : qconid
    { $1 }
 | '(' gconsym ')'
    { $2 }

qconop :: { String }
 : gconsym
    { $1 }

gconsym :: { String }
 : ':'
    { ":" }
 | qconsym 
    { $1 }

qconsym :: { String }
 : consym
    { $1 } 

tycon :: { String }
 : conid
    { $1 }

tyvar :: { String }
 : varid
    { $1 }

tycls :: { String }
 : conid
    { $1 }

qtycls :: { String }
  : tycls
    { $1 }

qconid :: { String }
  : conid
    { $1 }

qvarid :: { String }
  : varid  { $1 }

commas :: { String }
 : ','
    { "," }
 | commas ','
    { ',':$1 }

types_commasep :: { [Type] }
 : type
    { [$1] }
 | types_commasep ',' type
    { $1 ++ [$3] }

classes_commasep :: { [Class] }
 : class
    { [$1] }
 | classes_commasep ',' class
    { $1 ++ [$3] }

exps_commasep :: { [Exp] }
 : exp
    { [$1] }
 | exps_commasep ',' exp
    { $1 ++ [$3] }

pats_commasep :: { [Pat] }
 : pat
    { [$1] }
 | pats_commasep ',' pat
    { $1 ++ [$3] }

forallty :: { Type }
 : 'forall' tyvars '.' type
    { ForallT $2 [] $4 }
 | 'forall' tyvars '.' context '=>' type
    { ForallT $2 $4 $6 }

tyvars :: { [String] }
 : tyvar
    { [$1] }
 | tyvars tyvar
    { $1 ++ [$2] }

atypes :: { [Type] }
 : atype
    { [$1] }
 | atypes atype
    { $1 ++ [$2] }



{

data ParserState = ParserState {
    ps_text :: String,
    ps_line :: Integer,
    ps_column :: Integer,
    ps_error :: Maybe String
}

type ParserMonad = State ParserState

parseError :: Token -> ParserMonad a
parseError tok = do
    ln <- gets ps_line
    cl <- gets ps_column
    failE $ "error:" ++ show ln ++ ":" ++ show cl ++ ": parser error at " ++ show tok

data Token = 
       TokenOpenBracket
     | TokenCloseBracket
     | TokenOpenParen
     | TokenCloseParen
     | TokenOpenBrace
     | TokenCloseBrace
     | TokenDashArrow
     | TokenBindArrow
     | TokenEqualsArrow
     | TokenComma
     | TokenSemicolon
     | TokenPeriod
     | TokenBar
     | TokenEquals
     | TokenAtSign
     | TokenPercent
     | TokenHash
     | TokenColon
     | TokenBackSlash
     | TokenDoubleColon
     | TokenConId String
     | TokenVarId String
     | TokenVarSym String
     | TokenConSym String
     | TokenInteger Integer
     | TokenData
     | TokenForall
     | TokenClass
     | TokenInstance
     | TokenWhere
     | TokenCase
     | TokenOf
     | TokenIf
     | TokenThen
     | TokenElse
     | TokenDo
     | TokenEOF
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

isSymbol :: Char -> Bool
isSymbol c = c `elem` ":!#$%&*+./<=>?@\\^|-~"

singles :: [(Char, Token)]
singles = [
    ('[', TokenOpenBracket),
    (']', TokenCloseBracket),
    ('(', TokenOpenParen),
    (')', TokenCloseParen),
    ('{', TokenOpenBrace),
    ('}', TokenCloseBrace),
    (',', TokenComma),
    (';', TokenSemicolon)
    ]

reservedops :: [(String, Token)] 
reservedops = [
    (".", TokenPeriod),
    ("|", TokenBar),
    ("=", TokenEquals),
    ("@", TokenAtSign),
    ("%", TokenPercent),
    ("#", TokenHash),
    (":", TokenColon),
    ("\\", TokenBackSlash),
    ("->", TokenDashArrow),
    ("<-", TokenBindArrow),
    ("=>", TokenEqualsArrow),
    ("::", TokenDoubleColon)
    ]

keywords :: [(String, Token)]
keywords = [         
    ("data", TokenData),
    ("forall", TokenForall),
    ("class", TokenClass),
    ("instance", TokenInstance),
    ("where", TokenWhere),
    ("case", TokenCase),
    ("of", TokenOf),
    ("if", TokenIf),
    ("then", TokenThen),
    ("else", TokenElse),
    ("do", TokenDo)
    ]

failE :: String -> ParserMonad a
failE msg = do
    modify $ \ps -> ps { ps_error = Just msg}
    fail msg

-- advance a single column
single :: ParserMonad ()
single = modify $ \ps -> ps {ps_column = 1 + ps_column ps }

-- advance many columns
many :: String -> ParserMonad ()
many = mapM_ (const single)

-- advance to the next line
newline :: ParserMonad ()
newline = modify $ \ps -> ps {ps_line = 1 + ps_line ps, ps_column = 0 }

setText :: String -> ParserMonad ()
setText txt = modify $ \ps -> ps {ps_text = txt }

lexer :: (Token -> ParserMonad a) -> ParserMonad a
lexer output = do
  let osingle t r = single >> setText r >> output t
  text <- gets ps_text
  case text of
      [] -> output TokenEOF
      (c:cs) | (c `elem` (map fst singles)) ->
          osingle (fromJust (lookup c singles)) cs
      ('\n':cs) -> newline >> setText cs >> lexer output
      (c:cs) | isSpace c -> single >> setText cs >> lexer output
      (c:cs) | isLarge c ->
          let (ns, rest) = span isIdChar cs
          in many (c:ns) >> setText rest >> (output $ TokenConId (c:ns))
      (c:cs) | isSmall c ->
          let (ns, rest) = span isIdChar cs
          in case (c:ns) of
              kw | kw `elem` (map fst keywords) ->
                  many kw >> setText rest >> output (fromJust (lookup kw keywords))
              id -> many id >> setText rest >> output (TokenVarId id)
      (c:cs) | isDigit c ->
          let (ns, rest) = span isDigit cs
          in many (c:ns) >> setText rest >> output (TokenInteger (read (c:ns)))
      (c:cs) | isSymbol c ->
          let (ns, rest) = span isSymbol cs
          in case (c:ns) of
              "--" -> setText (dropWhile (/= '\n') rest) >> lexer output
              rop | rop `elem` (map fst reservedops) ->
                  many rop >> setText rest >> output (fromJust (lookup rop reservedops))
              op | head op == ':' -> many op >> setText rest >> output (TokenConSym op)
              op -> many op >> setText rest >> output (TokenVarSym op)
      cs -> failE $ "fail to lex: " ++ cs


data PDec =
    PDec Dec
  | PSig Sig
  | PClause Name Clause

isPClause :: PDec -> Bool
isPClause (PClause {}) = True
isPClause _ = False

coalesce :: (Monad m) => [PDec] -> m [Dec]
coalesce [] = return []
coalesce ((PSig s):ds) = do
    let (ms, rds) = span isPClause ds
    -- TODO: verify the names for each clauses matches the name for the sig.
    let d = ValD s (clauseE [c | PClause _ c <- ms]) 
    rest <- coalesce rds
    return (d:rest)
coalesce ((PDec d):ds) = do
    rest <- coalesce ds
    return (d:rest)

parseDecs :: (Monad m) => String -> m [Dec]
parseDecs text =
    case (runState seri_decls (ParserState text 1 0 Nothing)) of
        (ds, ParserState _ _ _ Nothing) -> coalesce ds
        (_, ParserState _ _ _ (Just msg)) -> fail msg
}

