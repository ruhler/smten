
{

-- vim: ft=haskell
module Seri.Lambda.Parser (
    -- * The Seri Language
    -- $doc
    parse
    ) where

import Control.Monad.State

import Data.Char hiding (isSymbol)
import Data.Maybe

import Seri.Failable
import Seri.Lambda.IR
import Seri.Lambda.Sugar

}

%name seri_module module
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
       ':'      { TokenColon }
       '\\'      { TokenBackSlash }
       '::'      { TokenDoubleColon }
       conid    { TokenConId $$ }
       varid    { TokenVarId $$ }
       varsym   { TokenVarSym $$ }
       consym   { TokenConSym $$ }
       integer  { TokenInteger $$ }
       'data'   { TokenData }
       'class'  { TokenClass }
       'instance'  { TokenInstance }
       'where'  { TokenWhere }
       'let'  { TokenLet }
       'in'  { TokenIn }
       'case'   { TokenCase }
       'of'     { TokenOf }
       'if'     { TokenIf }
       'then'     { TokenThen }
       'else'     { TokenElse }
       'do'     { TokenDo }
       'module' { TokenModule }
       'import' { TokenImport }

%%

module :: { Module }
 : 'module' modid 'where' body
    { Module $2 (fst $4) (snd $4) }

body :: { ([Import], [Dec]) }
 : '{' impdecls ';' topdecls opt(';') '}'
    { ($2, coalesce $4) }
 | '{' impdecls opt(';') '}'
    { ($2, []) }
 | '{' topdecls opt(';') '}'
    { ([], coalesce $2) }

impdecls :: { [Import] }
 : impdecl 
    { [$1] }
 | impdecls ';' impdecl
    { $1 ++ [$3] }

impdecl :: { Import }
 : 'import' modid
    { Import $2 }

topdecls :: { [PDec] }
 : topdecl
    { $1 }
 | topdecls ';' topdecl
    { $1 ++ $3 }

topdecl :: { [PDec] }
 : 'data' tycon opt(tyvars) '=' constrs
    { [PDec ds | ds <- recordD $2 (fromMaybe [] $3) $5] }
 | 'class' tycls tyvars 'where' '{' cdecls opt(';') '}'
    { [PDec (ClassD $2 $3 $6)] }
 | 'instance' opt(context) class 'where' '{' idecls opt(';') '}'
    { [PDec (InstD (fromMaybe [] $2) $3 (icoalesce $6))] }
 | decl
    { [$1] }

decl :: { PDec }
 : gendecl
    { PSig $1 }
 | funlhs rhs
    { PClause (fst $1) (Clause (snd $1) $2) }

cdecls :: { [TopSig] }
 : cdecl
    { [$1] }
 | cdecls ';' cdecl
    { $1 ++ [$3] }

cdecl :: { TopSig }
 : gendecl
    { $1 }

ldecls :: { [(Sig, Exp)] }
 : ldecl
    { [$1] }
 | ldecls ';' ldecl
    { $1 ++ [$3] }

ldecl :: { (Sig, Exp) }
 : var_typed rhs
    { ($1, $2) }

idecls :: { [(Name, Clause)] }
 : idecl
    { [$1] }
 | idecls ';' idecl
    { $1 ++ [$3] }

idecl :: { (Name, Clause) }
 : funlhs rhs
    { (fst $1, Clause (snd $1) $2) }

gendecl :: { TopSig }
 : var '::' type
    { TopSig $1 [] $3 }
 | var '::' context type
    { TopSig $1 $3 $4 }

type :: { Type }
 : btype
    { $1 } 
 | btype '->' type
    { AppT (AppT (ConT "->") $1) $3 }

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
 | '(' types_commasep ')'
    { foldl AppT (ConT $ "(" ++ replicate (length $2 - 1) ',' ++ ")") $2 }
 | '[' type ']'
    { AppT (ConT "[]") $2 }
 | '(' type ')'
    { $2 }

gtycon :: { String }
 : tycon
    { $1 }
 | '(' ')'
    { "()" }
 | '[' ']'
    { "[]" }
 | '(' '->' ')'
    { "->" }
 | '(' commas ')'
    { "(" ++ $2 ++ ")" }

context :: { [Class] }
 : '(' classes_commasep ')' '=>'
    { $2 }

class :: { Class }
 : qtycls atypes
    { Class $1 $2 }

constrs :: { [ConRec] }
 : constr
    { [$1] }
 | constrs '|' constr
    { $1 ++ [$3] }

constr :: { ConRec }
 : con opt(atypes)
    { NormalC $1 (fromMaybe [] $2) }
 | con '{' fielddecls '}'
    { RecordC $1 $3 }

fielddecls :: { [(Name, Type)] }
 : fielddecl
    { [$1] }
 | fielddecls ',' fielddecl
    { $1 ++ [$3] }

fielddecl :: { (Name, Type) }
 : var '::' type
    { ($1, $3) }

funlhs :: { (Name, [Pat]) }
 : var opt(apats)
    { ($1, fromMaybe [] $2) } 

rhs :: { Exp }
 : '=' exp
    { $2 }

exp :: { Exp }
 : exp10
    { $1 }
 | exp10 qop exp10
    { AppE (AppE $2 $1) $3 }

exp10 :: { Exp }
 : '\\' var_typed '->' exp
    { LamE $2 $4 }
 | 'let' '{' ldecls opt(';') '}' 'in' exp
    {% case attempt (letE $3 $7) of
         Right v -> return v    
         Left msg -> lfailE msg
    }
 | 'if' exp 'then' exp 'else' exp
    { ifE $2 $4 $6 }
 | 'case' exp 'of' '{' alts opt(';') '}'
    { CaseE $2 $5 }
 | 'do' '{' stmts exp ';' '}'
    { doE ($3 ++ [NoBindS $4]) }
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

alts :: { [Match] }
 : alt
    { [$1] }
 | alts ';' alt
    { $1 ++ [$3] }

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
    { ConP UnknownT ":" [$1, $3] }

pat10 :: { Pat }
 : gcon_typed apats
    { let Sig n t = $1 in ConP t n $2 }
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
    { let Sig n t = $1 in ConP t n [] }
 | integer
    { IntegerP $1 }
 | '(' pat ')'
    { $2 }
 | '(' pat ',' pats_commasep ')'
    { tupP ($2 : $4) }

gcon_typed :: { Sig }
 : '(' gcon '::' type ')'
    { Sig $2 $4 }
 | gcon
    { Sig $1 UnknownT }

gcon :: { String }
 : '(' ')'
    { "()" }
 | '[' ']'
    { "[]" }
 | '(' commas ')'
    { "(" ++ $2 ++ ")" }
 | qcon
    { $1 }

var_typed :: { Sig }
 : '(' var  '::' type ')'
    { Sig $2 $4 }
 | var
    { Sig $1 UnknownT }

var :: { String }
 : varid
    { $1 }
 | '(' varsym ')'
    { $2 }

qvar_withinfo :: { Exp }
 : '(' qvar '::' type ')'
    { VarE (Sig $2 $4) }
 | qvar
    { VarE (Sig $1 UnknownT) }

qvar :: { String }
 : qvarid
    { $1 }
 | '(' qvarsym ')'
    { $2 }

con :: { String }
 : conid
    { $1 }

qcon :: { String }
 : qconid
    { $1 }
 | '(' gconsym ')'
    { $2 }


qop :: { Exp }
 : qvarsym
    { VarE (Sig $1 UnknownT) }
 | qconop
    { ConE (Sig $1 UnknownT) }

qvarsym :: { String }
 : varsym
    { $1 }

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

modid :: { String }
 : conid
    { $1 }
 | modid '.' conid
    { $1 ++ "." ++ $3 }

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

opt(p)
 : p
    { Just $1 }
 |  -- empty
    { Nothing }


{

data PS = PS {
    ps_text :: String,
    ps_line :: Integer,
    ps_column :: Integer,
    ps_filename :: FilePath
}

type ParserMonad = StateT PS Failable

parseError :: Token -> ParserMonad a
parseError tok = lfailE $ "parser error at " ++ show tok

lfailE :: String -> ParserMonad a
lfailE msg = do
    ln <- gets ps_line
    cl <- gets ps_column
    fp <- gets ps_filename
    failE $ fp ++ ":" ++ show ln ++ ":" ++ show cl ++ ": " ++ msg

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
     | TokenLet
     | TokenIn
     | TokenCase
     | TokenOf
     | TokenIf
     | TokenThen
     | TokenElse
     | TokenDo
     | TokenModule
     | TokenImport
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
    ("class", TokenClass),
    ("instance", TokenInstance),
    ("where", TokenWhere),
    ("let", TokenLet),
    ("in", TokenIn),
    ("case", TokenCase),
    ("of", TokenOf),
    ("if", TokenIf),
    ("then", TokenThen),
    ("else", TokenElse),
    ("do", TokenDo),
    ("module", TokenModule),
    ("import", TokenImport)
    ]

failE :: String -> ParserMonad a
failE = lift . fail

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
  | PSig TopSig
  | PClause Name Clause

isPClause :: PDec -> Bool
isPClause (PClause {}) = True
isPClause _ = False

coalesce :: [PDec] -> [Dec]
coalesce [] = []
coalesce ((PSig s):ds) =
    let (ms, rds) = span isPClause ds
        rest = coalesce rds
        d = case ms of
                [] -> PrimD s
                _ -> ValD s (clauseE [c | PClause _ c <- ms]) 
    in (d:rest)
coalesce ((PDec d):ds) = d : coalesce ds

-- Merge clauses for the same method into a single method.
icoalesce :: [(Name, Clause)] -> [Method]
icoalesce [] = []
icoalesce ((n, c):ms) =
    let (me, rms) = span (\(n', _) -> n' == n) ms
        rest = icoalesce rms
        m = Method n (clauseE (c : map snd me))
    in (m : rest)

-- $doc
-- The Seri language is a subset of haskell. The following lists the
-- differences between the Seri language and haskell as defined in the 
-- Haskell 98 Language Report.
--
-- [@Things to implement eventually, perhaps@]
--
-- - Layout is not supported.
--
-- - Explicit module exports are not allowed.
--
-- - The module name must be specified explicitly. Main will not be inferred.
--
-- - Qualified imports are not allowed.
--
-- - Importing a module under a different name using 'as' is not allowed.
--
-- - Import specifications are not supported.
--
-- - Type synonyms
--
-- - Contexts in data declarations
--
-- - Deriving clauses in data declarations
--
-- - Newtype declarations
--
-- - Contexts in class declarations
--
-- - Default declarations.
--
-- - Pattern bindings (?)
--
-- - Empty class declarations.
--
-- - Default class methods implementations.
--
-- - Multiple vars in a type signature.
-- 
-- - fixity declarations.
--
-- - Qualified names.
--
-- - Unparenthesized contexts.
--
-- - ! in constructor declarations.
--
-- - infix constructors.
--
-- - 'where' clauses in expressions.
--
-- - pattern guards.
--
-- - expression type signatures.
--
-- - infix operations that require precedence knowledge.
--
-- - arithmetic sequences.
--
-- - list comprehension.
--
-- - left and right sections.
--
-- - labeled construction and update.
--
-- - let statements in do notation.
--
-- - list patterns
--
-- - irrefutable patterns
-- 
-- - variable operators, such as (a `foo` b).
--
-- - float, char, and string literals.
--
-- [@Things meant to be different from haskell@]
--
-- - Extra semicolons are often allowed.
--
-- - Multi-param type classes are supported.
--
-- - let expressions. These aren't supported because they require lazy
-- matching, which maybe we won't ever support in seri.
--
-- [@Things that may go away@]
--
-- - variables and constructors can be typed explicitly using a type signature
-- expression syntax. This is so pretty printed seri code with type
-- information can be parsed back in as is.


parse :: FilePath -> String -> Failable Module
parse fp text = do
    (m, _) <- runStateT seri_module (PS text 1 0 fp)
    return m

} 

