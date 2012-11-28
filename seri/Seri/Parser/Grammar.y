-- vim: ft=haskell
-------------------------------------------------------------------------------
-- Copyright (c) 2012      SRI International, Inc. 
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory under DARPA/AFRL contract (FA8750-10-C-0237)
-- ("CTSRD"), as part of the DARPA CRASH research programme.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
-------------------------------------------------------------------------------
--
-- Authors: 
--   Richard Uhler <ruhler@csail.mit.edu>
-- 
-------------------------------------------------------------------------------
{

module Seri.Parser.Grammar (parse) where

import Data.Maybe

import Seri.Failable
import Seri.Name
import Seri.Lit
import Seri.Type
import Seri.Sig
import Seri.Exp
import Seri.Dec
import Seri.Module
import Seri.Parser.Monad
import Seri.Parser.Lexer

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
       '+'      { TokenPlus }
       '-'      { TokenMinus }
       '*'      { TokenStar }
       '$'      { TokenDollar }
       '>>'     { TokenDoubleGT }
       '>>='    { TokenDoubleGTEQ }
       '||'     { TokenDoubleBar }
       '&&'     { TokenDoubleAmp }
       '=='     { TokenDoubleEq }
       '/='     { TokenSlashEq }
       '<'      { TokenLT }
       '<='     { TokenLE }
       '>='     { TokenGE }
       '>'      { TokenGT }
       '|'      { TokenBar }
       '='      { TokenEquals }
       ':'      { TokenColon }
       '#'      { TokenHash }
       '\\'      { TokenBackSlash }
       '::'      { TokenDoubleColon }
       conid    { TokenConId $$ }
       varid    { TokenVarId $$ }
       varsymt  { TokenVarSym $$ }
       consym   { TokenConSym $$ }
       integer  { TokenInteger $$ }
       char     { TokenChar $$ }
       string   { TokenString $$ }
       'data'   { TokenData }
       'type'   { TokenType }
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
       'deriving' { TokenDeriving }

%right '$'
%left '>>' '>>='
%right '||'
%right '&&'
%nonassoc '==' '/=' '<' '<=' '>=' '>'    
%left '+' '-'
%left '*'
%right '.'
%nonassoc qop

%%

module :: { Module }
 : 'module' modid 'where' body
    { let (is, sy, ds) = $4
      in Module (name $2) is sy ds }
 | body
    -- TODO: we should export only 'main' explicitly when explicit exports are
    -- supported
    { let (is, sy, ds) = $1
      in Module (name "Main") is sy ds}

body :: { ([Import], [Synonym], [Dec]) }
 : '{' impdecls ';' topdecls opt(';') '}'
    { let (syns, ds) = coalesce $4
      in ($2, syns, ds) }
 | '{' impdecls opt(';') '}'
    { ($2, [], []) }
 | '{' topdecls opt(';') '}'
    { let (syns, ds) = coalesce $2
      in ([], syns, ds) }

impdecls :: { [Import] }
 : impdecl 
    { [$1] }
 | impdecls ';' impdecl
    { $1 ++ [$3] }

impdecl :: { Import }
 : 'import' modid
    { Import (name $2) }

topdecls :: { [PDec] }
 : topdecl
    { $1 }
 | topdecls ';' topdecl
    { $1 ++ $3 }

topdecl :: { [PDec] }
 : 'data' tycon opt(tyvars) '=' opt(constrs) opt(deriving)
    { let { tyvars = fromMaybe [] $3;
            constrs = fromMaybe [] $5;
            derives = fromMaybe [] $6;
      } in [PDec ds | ds <- recordD (name $2) tyvars constrs derives]
    }
 | 'type' tycon '=' type
    { [PSynonym (Synonym (name $2) $4) ] }
 | 'class' tycls tyvars 'where' '{' cdecls opt(';') '}'
    { [PDec (ClassD (name $2) $3 $6)] }
 | 'instance' class 'where' '{' idecls opt(';') '}'
    { [PDec (InstD [] $2 (icoalesce $5))] }
 | 'instance' context class 'where' '{' idecls opt(';') '}'
    { [PDec (InstD $2 $3 (icoalesce $6))] }
 | decl
    { [$1] }

deriving :: { [String] }
 : 'deriving' '(' qtycls_commasep ')'
    { $3 }

decl :: { PDec }
 : gendecl
    { PSig $1 }
 | funlhs rhs
    { PClause (fst $1) (MMatch (snd $1) $2) }

cdecls :: { [TopSig] }
 : cdecl
    { [$1] }
 | cdecls ';' cdecl
    { $1 ++ [$3] }

cdecl :: { TopSig }
 : gendecl
    { $1 }

ldecls :: { [(Pat, Exp)] }
 : ldecl
    { [$1] }
 | ldecls ';' ldecl
    { $1 ++ [$3] }

ldecl :: { (Pat, Exp) }
 : pat rhs
    { ($1, $2) }

idecls :: { [(Name, MMatch)] }
 : idecl
    { [$1] }
 | idecls ';' idecl
    { $1 ++ [$3] }

idecl :: { (Name, MMatch) }
 : funlhs rhs
    { (fst $1, MMatch (snd $1) $2) }

gendecl :: { TopSig }
 : var '::' type
    { TopSig (name $1) [] $3 }
 | var '::' context type
    { TopSig (name $1) $3 $4 }

type :: { Type }
 : btype
    { $1 } 
 | btype '->' type
    { AppT (AppT (ConT (name "->")) $1) $3 }

btype :: { Type }
 : atype
    { $1 }
 | btype atype
    { AppT $1 $2 }

atype :: { Type }
 : gtycon
    { ConT (name $1) }
 | tyvarnm
    { VarT (name $1) }
 | '(' types_commasep ')'
    { tupleT $2 }     -- takes care of '(' type ')' case too.
 | '[' type ']'
    { AppT (ConT (name "[]")) $2 }
 | '#' antype
    { NumT $2 }

ntype :: { NType }
 : antype { $1 }
 | antype '+' antype { addNT $1 $3 }
 | antype '-' antype { subNT $1 $3 }
 | antype '*' antype { mulNT $1 $3 }

antype :: { NType }
 : integer
    { ConNT $1 }
 | tyvarnm
    { VarNT (name $1) }
 | '(' ntype ')'
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

-- context is treated as a btype to avoid conflicts like:
--      (Foo Bar) -> ...
-- vs.  (Foo Bar) => ...
--
context :: { [Class] }
 : btype '=>'
    {% case attempt $ mkContext $1 of
         Right x -> return x
         Left msg -> lfailE msg
    }

class :: { Class }
 : qtycls atypes
    { Class (name $1) $2 }

constrs :: { [ConRec] }
 : constr
    { [$1] }
 | constrs '|' constr
    { $1 ++ [$3] }

constr :: { ConRec }
 : con opt(atypes)
    { NormalC (name $1) (fromMaybe [] $2) }
 | con '{' opt(fielddecls) '}'
    { RecordC (name $1) (fromMaybe [] $3) }

fielddecls :: { [(Name, Type)] }
 : fielddecl
    { [$1] }
 | fielddecls ',' fielddecl
    { $1 ++ [$3] }

fielddecl :: { (Name, Type) }
 : var '::' type
    { (name $1, $3) }

funlhs :: { (Name, [Pat]) }
 : var opt(apats)
    { (name $1, fromMaybe [] $2) } 

rhs :: { Exp }
 : '=' exp
    { $2 }

exp :: { Exp }
 : lexp { $1 }
 | lexp '::' type { typeE $1 $3 }
 | exp '+' exp { addE $1 $3 }
 | exp '-' exp { subE $1 $3 }
 | exp '*' exp { mulE $1 $3 }
 | exp '$' exp { opE "$" $1 $3 }
 | exp '>>' exp { opE ">>" $1 $3 }
 | exp '>>=' exp { opE ">>=" $1 $3 }
 | exp '||' exp { opE "||" $1 $3 }
 | exp '&&' exp { opE "&&" $1 $3 }
 | exp '==' exp { opE "==" $1 $3 }
 | exp '/=' exp { opE "/=" $1 $3 }
 | exp '<' exp { opE "<" $1 $3 }
 | exp '<=' exp { opE "<=" $1 $3 }
 | exp '>=' exp { opE ">=" $1 $3 }
 | exp '>' exp { opE ">" $1 $3 }
 | exp qop exp { appsE $2 [$1, $3] }

lexp :: { Exp }
 : '\\' var_typed '->' exp
    { lamE $2 $4 }
 | 'let' '{' ldecls opt(';') '}' 'in' exp
    { mletsE $3 $7 }
 | 'if' exp 'then' exp 'else' exp
    { ifE $2 $4 $6 }
 | 'case' exp 'of' '{' alts opt(';') '}'
    { mcaseE $2 $5 }
 | 'do' '{' stmts opt(';') '}'
    {% case last $3 of
         NoBindS _ -> return $ doE $3
         _ -> lfailE "last statement in do must be an expression"
    }
 | fexp
    { $1 }

fexp :: { Exp }
 : aexp
    { $1 }
 | fexp aexp
    { appE $1 $2 }

aexp :: { Exp }
 : qvar
    { VarE (Sig (name $1) UnknownT) }
 | gcon
    { ConE (Sig (name $1) UnknownT) }
 | literal
    { $1 }
 | '(' exp ')'
    { $2 }
 | '(' exp ',' exps_commasep ')'
    { tupleE ($2 : $4) }
 | '['  exps_commasep ']'
    { listE $2 }
 | aexp '{' opt(fbinds) '}'
    { case $1 of
        ConE s -> recordC s (fromMaybe [] $3)
        x -> recordU x (fromMaybe [] $3)
    }

literal :: { Exp }
 : integer
    { numberE $1 }
 | char
    { charE $1 }
 | string
    { stringE $1 }

alts :: { [SMatch] }
 : alt
    { [$1] }
 | alts ';' alt
    { $1 ++ [$3] }

alt :: { SMatch }
 : pat '->' exp
    { SMatch $1 $3 }

stmts :: { [Stmt] }
 : stmt 
    { [$1] }
 | stmts ';' stmt
    { $1 ++ [$3] }

stmt :: { Stmt }
 : var '<-' exp
    { BindS (VarP (name $1)) $3 }
 | exp 
    { NoBindS $1 }
 | 'let' '{' ldecl opt(';') '}'
    { LetS (fst $3) (snd $3) }

fbinds :: { [(Name, Exp)] }
 : fbind 
    { [$1] }
 | fbinds ',' fbind
    { $1 ++ [$3] }

fbind :: { (Name, Exp) }
 : qvar '=' exp
    { (name $1, $3) }


pat :: { Pat }
 : pat10
    { $1 }
 | pat10 ':' pat
    { ConP (name ":") [$1, $3] }

pat10 :: { Pat }
 : gcon apats
    { ConP (name $1) $2 }
 | apat
    { $1 }

apats :: { [Pat] }
 : apat
    { [$1] }
 | apats apat
    { $1 ++ [$2] }

apat :: { Pat }
 : var
    { if $1 == "_" then WildP else VarP (name $1) }
 | gcon
    { ConP (name $1) [] }
 | integer
    { LitP (integerL $1) }
 | char
    { LitP (charL $1) }
 | '(' pat ')'
    { $2 }
 | '(' pat ',' pats_commasep ')'
    { tupleP ($2 : $4) }
 | '[' pats_commasep ']'
    { listP $2 }

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
    { Sig (name $2) $4 }
 | var
    { Sig (name $1) UnknownT }

var :: { String }
 : varid
    { $1 }
 | '(' varsym ')'
    { $2 }
 | '(' varsym_op ')'
    { $2 }

qvar :: { String }
 : qvarid
    { $1 }
 | '(' qvarsym ')'
    { $2 }
 | '(' varsym_op ')'
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
    { VarE (Sig (name $1) UnknownT) }
 | qconop
    { ConE (Sig (name $1) UnknownT) }

qvarsym :: { String }
 : varsym
    { $1 }

varsym_op :: { String }
 : '+' { "+" }
 | '-' { "-" }
 | '*' { "*" }
 | '$'  { "$" }
 | '>>' { ">>" }
 | '>>=' { ">>=" }
 | '||' { "||" }
 | '&&' { "&&" }
 | '==' { "==" }
 | '/=' { "/=" }
 | '<'  { "<" }
 | '<=' { "<=" }
 | '>=' { ">=" }
 | '>'  { ">" }

varsym :: { String }
 : varsymt
    { $1 }
 | '.' { "." }

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

tyvarnm :: { String }
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

qtycls_commasep :: { [String] }
 : qtycls 
    { [$1] }
 | qtycls_commasep ',' qtycls
    { $1 ++ [$3] }

types_commasep :: { [Type] }
 : type
    { [$1] }
 | types_commasep ',' type
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

tyvar :: { TyVar }
 : tyvarnm
    { NormalTV (name $1) }
 | '#' tyvarnm
    { NumericTV (name $2) }

tyvars :: { [TyVar] }
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

parseError :: Token -> ParserMonad a
parseError tok = lfailE $ "parser error at " ++ show tok

data PDec =
    PDec Dec
  | PSig TopSig
  | PClause Name MMatch
  | PSynonym Synonym

isPClause :: PDec -> Bool
isPClause (PClause {}) = True
isPClause _ = False

coalesce :: [PDec] -> ([Synonym], [Dec])
coalesce [] = ([], [])
coalesce ((PSig s):ds) =
    let (ms, rds) = span isPClause ds
        (syns, rest) = coalesce rds
        d = case ms of
                [] -> PrimD s
                _ -> ValD s (clauseE [c | PClause _ c <- ms]) 
    in (syns, d:rest)
coalesce ((PDec d):ds) =
   let (syns, rest) = coalesce ds
   in (syns, d:rest)
coalesce ((PSynonym s):ds) =
   let (syns, rest) = coalesce ds
   in (s:syns, rest)

-- Merge clauses for the same method into a single method.
icoalesce :: [(Name, MMatch)] -> [Method]
icoalesce [] = []
icoalesce ((n, c):ms) =
    let (me, rms) = span (\(n', _) -> n' == n) ms
        rest = icoalesce rms
        m = Method n (clauseE (c : map snd me))
    in (m : rest)

-- A context is parsed first as a type to avoid a reduce/reduce conflict. Here
-- we turn that type back into a proper context.
mkContext :: Type -> Failable [Class] 
mkContext t = 
  let mkclass :: Type -> Failable Class
      mkclass t =
        case de_appsT t of
          (ConT nm, ts) -> return $ Class nm ts
          _ -> throw $ "invalid context"

      classes = fromMaybe [t] (de_tupleT t)
  in mapM mkclass classes
      
      

parse :: FilePath -> String -> Failable Module
parse = runParser seri_module
} 

