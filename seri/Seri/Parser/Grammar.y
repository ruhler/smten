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
import Seri.Parser.PatOrExp
import Seri.Parser.Utils

}

%name seri_module module
%tokentype { Token }
%error { pfailE }
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
       '@'      { TokenAt }
       '..'      { TokenDoubleDot }
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
%right ':'
%nonassoc '==' '/=' '<' '<=' '>=' '>'    
%left '+' '-'
%left '*'
%right '.'
%nonassoc op

%%

module :: { Module }
 : 'module' modid 'where' body
    { let (is, sy, ds) = $4
      in Module $2 is sy ds }
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
    { Import $2 }

topdecls :: { [PDec] }
 : topdecl
    { $1 }
 | topdecls ';' topdecl
    { $1 ++ $3 }

topdecl :: { [PDec] }
 : 'data' tycon lopt(tyvars) '=' lopt(constrs) lopt(deriving)
    { [PDec ds | ds <- recordD $2 $3 $5 $6] }
 | 'type' tycon '=' type
    { [PSynonym (Synonym $2 $4) ] }
 | 'class' tycon tyvars 'where' '{' cdecls opt(';') '}'
    { [PDec (ClassD $2 $3 $6)] }
 | 'instance' class 'where' '{' idecls opt(';') '}'
    { [PDec (InstD [] $2 (icoalesce $5))] }
 | 'instance' context class 'where' '{' idecls opt(';') '}'
    { [PDec (InstD $2 $3 (icoalesce $6))] }
 | decl
    { [$1] }

deriving :: { [Name] }
 : 'deriving' '(' tycls_commasep ')'
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

ldecls :: { [LDec] }
 : ldecl
    { [$1] }
 | ldecls ';' ldecl
    { $1 ++ [$3] }

ldecl :: { LDec }
 : aexp lopt(aexps) rhs {% do
      p <- toPat $1
      ps <- mapM toPat $2
      case (p, ps) of
        (p, []) -> return (LPat p $3)
        (VarP n, ps) -> return (LClause n (MMatch ps $3))
        _ -> lfailE "invalid let declaration"
    }

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
    { TopSig $1 [] $3 }
 | var '::' context type
    { TopSig $1 $3 $4 }

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
    { ConT $1 }
 | tyvarnm
    { VarT $1 }
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
    { VarNT $1 }
 | '(' ntype ')'
    { $2 }

gtycon :: { Name }
 : tycon
    { $1 }
 | '(' ')'
    { name "()" }
 | '[' ']'
    { name "[]" }
 | '(' '->' ')'
    { name "->" }
 | '(' commas ')'
    { name $ "(" ++ $2 ++ ")" }

-- context is treated as a btype to avoid conflicts like:
--      (Foo Bar) -> ...
-- vs.  (Foo Bar) => ...
context :: { [Class] }
 : btype '=>'
    {% mkContext $1 }

class :: { Class }
 : tycon atypes
    { Class $1 $2 }

constrs :: { [ConRec] }
 : constr
    { [$1] }
 | constrs '|' constr
    { $1 ++ [$3] }

constr :: { ConRec }
 : con lopt(atypes)
    { NormalC $1 $2 }
 | con '{' lopt(fielddecls) '}'
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
 : var lopt(aexps)
    {% fmap ((,) $1) (mapM toPat $2) } 

rhs :: { Exp }
 : '=' exp
    {% toExp $2 }

exp :: { PatOrExp }
 : lexp { $1 }
 | lexp '::' type { typePE $1 $3 }
 | exp '+' exp { opPE "+" $1 $3 }
 | exp '-' exp { opPE "-" $1 $3 }
 | exp '*' exp { opPE "*" $1 $3 }
 | exp '$' exp { opPE "$" $1 $3 }
 | exp '>>' exp { opPE ">>" $1 $3 }
 | exp '>>=' exp { opPE ">>=" $1 $3 }
 | exp '||' exp { opPE "||" $1 $3 }
 | exp '&&' exp { opPE "&&" $1 $3 }
 | exp ':' exp { conopPE ":"$1 $3 }
 | exp '==' exp { opPE "==" $1 $3 }
 | exp '/=' exp { opPE "/=" $1 $3 }
 | exp '<' exp { opPE "<" $1 $3 }
 | exp '<=' exp { opPE "<=" $1 $3 }
 | exp '>=' exp { opPE ">=" $1 $3 }
 | exp '>' exp { opPE ">" $1 $3 }
 | exp op exp { appsPE $2 [$1, $3] }

lexp :: { PatOrExp }
 : '\\' var '->' exp
    { lamPE (Sig $2 UnknownT) $4 }
 | 'let' '{' ldecls opt(';') '}' 'in' exp
    { letPE $3 $7 }
 | 'if' exp 'then' exp 'else' exp
    { ifPE $2 $4 $6 }
 | 'case' exp 'of' '{' alts opt(';') '}'
    { casePE $2 $5 }
 | 'do' '{' stmts opt(';') '}'
    {% case last $3 of
         NoBindS _ -> return (doPE $3)
         _ -> lfailE "last statement in do must be an expression"
    }
 | aexps
    { appsPE (head $1) (tail $1) }

aexps :: { [PatOrExp] }
 : aexp
    { [$1] }
 | aexps aexp
    { $1 ++ [$2] }

aexp :: { PatOrExp }
 : var
    { varPE $1 }
 | var '@' aexp
    { asPE $1 $3 }
 | gcon
    { conPE $1 }
 | literal
    { $1 }
 | '(' exp ')'
    { $2 }
 | '(' exp ',' exps_commasep ')'
    { tuplePE ($2 : $4) }
 | '[' exp '..' exp ']'
    { fromtoPE $2 $4 }
 | '[' exp '|' quals ']'
    { lcompPE $2 $4 }
 | '['  exps_commasep ']'
    { listPE $2 }
 | aexp '{' lopt(fbinds) '}'
    { updatePE $1 $3 }

qual :: { Qual }
 : exp '<-' exp {% do
     p <- toPat $1
     e <- toExp $3
     return (QGen p e)
   }
 | exp
    {% fmap QGuard (toExp $1) }
 | 'let' ldecls
    { QBind (lcoalesce $2) }

quals :: { [Qual] }
 : qual
    { [$1] }
 | quals ',' qual
    { $1 ++ [$3] }

literal :: { PatOrExp }
 : integer
    { integerPE $1 }
 | char
    { charPE $1 }
 | string
    { stringPE $1 }

alts :: { [SMatch] }
 : alt
    { [$1] }
 | alts ';' alt
    { $1 ++ [$3] }

alt :: { SMatch }
 : exp '->' exp {% do
    p <- toPat $1
    e <- toExp $3
    return (SMatch p e)
  }

stmts :: { [Stmt] }
 : stmt 
    { [$1] }
 | stmts ';' stmt
    { $1 ++ [$3] }

stmt :: { Stmt }
 : exp '<-' exp {% do
    p <- toPat $1
    e <- toExp $3
    return (BindS p e)
   }
 | exp 
    {% fmap NoBindS (toExp $1) }
 | 'let' '{' ldecls opt(';') '}'
    { LetS (lcoalesce $3) }

fbinds :: { [(Name, Exp)] }
 : fbind 
    { [$1] }
 | fbinds ',' fbind
    { $1 ++ [$3] }

fbind :: { (Name, Exp) }
 : var '=' exp
    {% fmap ((,) $1) (toExp $3) }

gcon :: { Name }
 : '(' ')'
    { name "()" }
 | '[' ']'
    { name "[]" }
 | '(' commas ')'
    { name $ "(" ++ $2 ++ ")" }
 | qcon
    { $1 }

var :: { Name }
 : varid
    { $1 }
 | '(' varsym ')'
    { $2 }
 | '(' varsym_op ')'
    { $2 }

con :: { Name }
 : conid
    { $1 }

qcon :: { Name }
 : conid
    { $1 }
 | '(' consym ')'
    { $2 }
 | '(' consym_op ')'
    { $2 }

consym_op :: { Name }
 : ':' { name ":" }


op :: { PatOrExp }
 : varsym
    { varPE $1 }
 | consym
    { conPE $1 }

varsym_op :: { Name }
 : '+' { name "+" }
 | '-' { name "-" }
 | '*' { name "*" }
 | '$'  { name "$" }
 | '>>' { name ">>" }
 | '>>=' { name ">>=" }
 | '||' { name "||" }
 | '&&' { name "&&" }
 | '==' { name "==" }
 | '/=' { name "/=" }
 | '<'  { name "<" }
 | '<=' { name "<=" }
 | '>=' { name ">=" }
 | '>'  { name ">" }

varsym :: { Name }
 : varsymt
    { $1 }
 | '.' { name "." }

tyvarnm :: { Name }
 : varid
    { $1 }

tycon :: { Name }
 : conid
    { $1 }

modid :: { Name }
 : conid
    { $1 }
 | modid '.' conid
    { $1 `nappend` name "." `nappend` $3 }

commas :: { String }
 : ','
    { "," }
 | commas ','
    { ',':$1 }

tycls_commasep :: { [Name] }
 : tycon 
    { [$1] }
 | tycls_commasep ',' tycon
    { $1 ++ [$3] }

types_commasep :: { [Type] }
 : type
    { [$1] }
 | types_commasep ',' type
    { $1 ++ [$3] }

exps_commasep :: { [PatOrExp] }
 : exp
    { [$1] }
 | exps_commasep ',' exp
    { $1 ++ [$3] }

tyvar :: { TyVar }
 : tyvarnm
    { NormalTV $1 }
 | '#' tyvarnm
    { NumericTV $2 }

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

lopt(p)
 : opt(p)
    { fromMaybe [] $1 }


{

parse :: FilePath -> String -> Failable Module
parse = runParser seri_module

} 

