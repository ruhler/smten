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

module Seri.Lambda.Parser.Grammar (parse) where

import Data.Maybe

import Seri.Failable
import Seri.Lambda.IR
import Seri.Lambda.Modularity
import Seri.Lambda.Prelude
import Seri.Lambda.Sugar
import Seri.Lambda.Types

import Seri.Lambda.Parser.Monad
import Seri.Lambda.Parser.Lexer

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

%%

module :: { Module }
 : 'module' modid 'where' body
    { Module (name $2) (fst $4) (snd $4) }

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
    { PClause (fst $1) (Clause (snd $1) $2) }

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
    { tupT $2 }     -- takes care of '(' type ')' case too.
 | '[' type ']'
    { AppT (ConT (name "[]")) $2 }
 | '#' antype
    { NumT $2 }

ntype :: { NType }
 : antype 
    { $1 }
 | antype varsym antype
    { AppNT $2 $1 $3 }

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
 | con '{' fielddecls '}'
    { RecordC (name $1) $3 }

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
 : exp10
    { $1 }
 | exp10 qop exp10
    { AppE (AppE $2 $1) $3 }

exp10 :: { Exp }
 : '\\' var_typed '->' exp
    { LamE $2 $4 }
 | 'let' '{' ldecls opt(';') '}' 'in' exp
    { letE $3 $7 }
 | 'if' exp 'then' exp 'else' exp
    { ifE $2 $4 $6 }
 | 'case' exp 'of' '{' alts opt(';') '}'
    { CaseE $2 $5 }
 | 'do' '{' stmts '}'
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
    { AppE $1 $2 }

aexp :: { Exp }
 : qvar_withinfo
    { $1 }
 | gcon_typed
    { ConE $1 }
 | literal
    { $1 }
 | '(' exp ')'
    { $2 }
 | '(' exp ',' exps_commasep ')'
    { tupE ($2 : $4) }
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
 : var_typed '<-' exp ';'
    { BindS $1 $3 }
 | exp ';'
    { NoBindS $1 }

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
    { ConP UnknownT (name ":") [$1, $3] }

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
    { let Sig n t = $1 in if n == (name "_") then WildP t else VarP $1 }
 | gcon_typed
    { let Sig n t = $1 in ConP t n [] }
 | integer
    { LitP (IntegerL $1) }
 | char
    { LitP (CharL $1) }
 | '(' pat ')'
    { $2 }
 | '(' pat ',' pats_commasep ')'
    { tupP ($2 : $4) }
 | '[' pats_commasep ']'
    { listP $2 }

gcon_typed :: { Sig }
 : '(' gcon '::' type ')'
    { Sig (name $2) $4 }
 | gcon
    { Sig (name $1) UnknownT }

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

qvar_withinfo :: { Exp }
 : '(' qvar '::' type ')'
    { VarE (Sig (name $2) $4) }
 | qvar
    { VarE (Sig (name $1) UnknownT) }

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
    { VarE (Sig (name $1) UnknownT) }
 | qconop
    { ConE (Sig (name $1) UnknownT) }

qvarsym :: { String }
 : varsym
    { $1 }

varsym :: { String }
 : varsymt
    { $1 }
 | '.'
    { "." }

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

-- A context is parsed first as a type to avoid a reduce/reduce conflict. Here
-- we turn that type back into a proper context.
mkContext :: Type -> Failable [Class] 
mkContext t = 
  let mkclass :: Type -> Failable Class
      mkclass t =
        case unappsT t of
          (ConT nm):ts -> return $ Class nm ts
          _ -> fail $ "invalid context"

      classes = untupT t
  in mapM mkclass classes
      
      

parse :: FilePath -> String -> Failable Module
parse = runParser seri_module
} 

