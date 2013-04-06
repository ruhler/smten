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

module Smten.Parser.Grammar (parse) where

import Data.Maybe

import Smten.Location
import Smten.Failable
import Smten.Name
import Smten.Lit
import Smten.Type
import Smten.Sig
import Smten.Exp
import Smten.Dec
import Smten.Module
import Smten.Parser.Monad
import Smten.Parser.Lexer
import Smten.Parser.PatOrExp
import Smten.Parser.Utils

}

%name smten_module module
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
       '`'      { TokenBackTick }
       '..'      { TokenDoubleDot }
       '\\'      { TokenBackSlash }
       '::'      { TokenDoubleColon }
       conid    { TokenConId $$ }
       varid_   { TokenVarId $$ }
       varsym  { TokenVarSym $$ }
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
       'qualified' { TokenQualified }
       'as' { TokenAs }
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
 : 'module' qconid 'where' mbody
    { let (is, sy, dds, drv, ds) = $4
      in Module $2 is sy dds drv ds }
 | mbody
    -- TODO: we should export only 'main' explicitly when explicit exports are
    -- supported
    { let (is, sy, dds, drv, ds) = $1
      in Module (name "Main") is sy dds drv ds}

mbody :: { ([Import], [Synonym], [DataDec], [Deriving], [Dec]) }
 : '{' impdecls ';' topdecls opt(';') '}'
    { let (syns, dds, drv, ds) = coalesce $4
      in ($2, syns, dds, drv, ds) }
 | '{' impdecls opt(';') '}'
    { ($2, [], [], [], []) }
 | '{' topdecls opt(';') '}'
    { let (syns, dds, drv, ds) = coalesce $2
      in ([], syns, dds, drv, ds) }

impdecls :: { [Import] }
 : impdecl 
    { [$1] }
 | impdecls ';' impdecl
    { $1 ++ [$3] }

impdecl :: { Import }
 : 'import' opt('qualified') qconid opt(asmod)
    { Import $3 (fromMaybe $3 $4) (isJust $2) }

asmod :: { Name }
 : 'as' qconid { $2 }

topdecls :: { [PDec] }
 : topdecl
    { $1 }
 | topdecls ';' topdecl
    { $1 ++ $3 }

topdecl :: { [PDec] }
 : 'data' conid lopt(tyvars) '=' lopt(constrs) lopt(deriving)
    {% withloc $ \l ->
         PDataDec (DataDec $2 $3 $5) : [PDec ds | ds <- recordD l $2 $3 $5 $6] }
 | 'type' conid lopt(tyvarnms) '=' type
    { [PSynonym (Synonym $2 $3 $5) ] }
 | 'class' conid tyvars 'where' '{' cdecls opt(';') '}'
    {% withloc $ \l -> [PDec (ClassD l [] $2 $3 (ccoalesce $6))] }
 | 'class' context conid tyvars 'where' '{' cdecls opt(';') '}'
    {% withloc $ \l -> [PDec (ClassD l $2 $3 $4 (ccoalesce $7))] }
 | 'instance' class 'where' '{' idecls opt(';') '}'
    {% withloc $ \l -> [PDec (InstD l [] $2 (icoalesce $5))] }
 | 'instance' context class 'where' '{' idecls opt(';') '}'
    {% withloc $ \l -> [PDec (InstD l $2 $3 (icoalesce $6))] }
 | 'deriving' 'instance' class
    {% withloc $ \l -> [PDeriving (Deriving l [] $3)] }
 | 'deriving' 'instance' context class
    {% withloc $ \l -> [PDeriving (Deriving l $3 $4)] }
 | decl
    { [$1] }

deriving :: { [Name] }
 : 'deriving' '(' tycls_commasep ')'
    { $3 }

decl :: { PDec }
 : gendecl
    {% withloc $ \l ->  PSig l $1 }
 | funlhs rhs
    { PClause (fst $1) (MAlt (snd $1) $2) }

cdecls :: { [CDec] }
 : cdecl
    { [$1] }
 | cdecls ';' cdecl
    { $1 ++ [$3] }

cdecl :: { CDec }
 : gendecl
    {% withloc $ \l ->  CSig l $1 }
 | funlhs rhs
    { CClause (fst $1) (MAlt (snd $1) $2) }

ldecls :: { [LDec] }
 : ldecl
    { [$1] }
 | ldecls ';' ldecl
    { $1 ++ [$3] }

ldecl :: { LDec }
 : apoe lopt(apoes) rhs {% withlocM $ \l -> do
      p <- toPat $1
      ps <- mapM toPat $2
      case (p, ps, $3) of
        (p, [], WBodies _ [Body _ [] e] []) -> return (LPat p e)
        (VarP n, _, _) -> return (LClause l n (MAlt ps $3))
        _ -> lfailE "invalid let declaration"
    }

idecls :: { [(Name, Location, MAlt)] }
 : idecl
    { [$1] }
 | idecls ';' idecl
    { $1 ++ [$3] }

idecl :: { (Name, Location, MAlt) }
 : funlhs rhs
    {%withloc $ \l ->  (fst $1, l, MAlt (snd $1) $2) }

gendecl :: { TopSig }
 : var '::' type
    { TopSig $1 [] $3 }
 | var '::' context type
    { TopSig $1 $3 $4 }

type :: { Type }
 : btype
    { $1 } 
 | btype '->' type
    { arrowT $1 $3 }

btype :: { Type }
 : atype
    { $1 }
 | btype atype
    { AppT $1 $2 }

atype :: { Type }
 : gtycon
    { conT $1 }
 | varid
    { VarT $1 UnknownK }
 | '(' types_commasep ')'
    { tupleT $2 }     -- takes care of '(' type ')' case too.
 | '[' type ']'
    { AppT (conT (name "[]")) $2 }
 | '#' antype
    { $2 }

ntype :: { Type }
 : antype { $1 }
 | antype '+' antype { addNT $1 $3 }
 | antype '-' antype { subNT $1 $3 }
 | antype '*' antype { mulNT $1 $3 }

antype :: { Type }
 : integer
    { NumT $1 }
 | varid
    { VarT $1 NumK }
 | '(' ntype ')'
    { $2 }

gtycon :: { Name }
 : conid
    { $1 }
 | '(' ')'
    { unitN }
 | '[' ']'
    { name "[]" }
 | '(' '->' ')'
    { arrowN }
 | '(' commas ')'
    { name $ "(" ++ $2 ++ ")" }

-- context is treated as a btype to avoid conflicts like:
--      (Foo Bar) -> ...
-- vs.  (Foo Bar) => ...
context :: { [Class] }
 : btype '=>'
    {% mkContext $1 }

class :: { Class }
 : conid atypes
    { Class $1 $2 }

constrs :: { [ConRec] }
 : constr
    { [$1] }
 | constrs '|' constr
    { $1 ++ [$3] }

constr :: { ConRec }
 : conid lopt(atypes)
    { NormalC $1 $2 }
 | conid '{' lopt(fielddecls) '}'
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
 : var lopt(apoes)
    {% fmap ((,) $1) (mapM toPat $2) } 

rhs :: { WBodies }
 : '=' poe lopt(wdecls) {% withlocM $ \l -> do
    e <- toExp $2
    return $ WBodies l [Body l [] e] $3
   }
 | rhsbodies lopt(wdecls)
    {% withloc $ \l -> WBodies l $1 $2 }

wdecls :: { [(Pat, Exp)] }
 : 'where' '{' ldecls opt(';') '}'
    { lcoalesce $3 }

rhsbodies :: { [Body] }
 : rhsbody { [$1] }
 | rhsbodies rhsbody { $1 ++ [$2] }

rhsbody :: { Body }
 : '|' guards '=' poe
    {% withlocM $ \l -> fmap (Body l $2) (toExp $4) }

poe :: { PatOrExp }
 : lpoe { $1 }
 | lpoe '::' type {% withloc $ \l -> sigPE l $1 $3 }
 | poe '+' poe {% lopPE "+" $1 $3 }
 | poe '-' poe {% lopPE "-" $1 $3 }
 | poe '*' poe {% lopPE "*" $1 $3 }
 | poe '$' poe {% lopPE "$" $1 $3 }
 | poe '.' poe {% lopPE "." $1 $3 }
 | poe '>>' poe {% lopPE ">>" $1 $3 }
 | poe '>>=' poe {% lopPE ">>=" $1 $3 }
 | poe '||' poe {% lopPE "||" $1 $3 }
 | poe '&&' poe {% lopPE "&&" $1 $3 }
 | poe ':' poe {% withloc $ \l -> conopPE l ":"$1 $3 }
 | poe '==' poe {% lopPE "==" $1 $3 }
 | poe '/=' poe {% lopPE "/=" $1 $3 }
 | poe '<' poe {% lopPE "<" $1 $3 }
 | poe '<=' poe {% lopPE "<=" $1 $3 }
 | poe '>=' poe {% lopPE ">=" $1 $3 }
 | poe '>' poe {% lopPE ">" $1 $3 }
 | poe op poe {% withloc $ \l -> appsPE l $2 [$1, $3] }

lpoe :: { PatOrExp }
 : '\\' apoes '->' poe
    {% withloc $ \l -> lamPE l $2 $4 }
 | 'let' '{' ldecls opt(';') '}' 'in' poe
    {% withloc $ \l -> letPE l $3 $7 }
 | 'if' poe 'then' poe 'else' poe
    {% withloc $ \l -> ifPE l $2 $4 $6 }
 | 'case' poe 'of' '{' alts opt(';') '}'
    {% withloc $ \l -> casePE l $2 $5 }
 | 'do' '{' stmts opt(';') '}'
    {% case last $3 of
         NoBindS _ -> withloc $ \l -> (doPE l $3)
         _ -> lfailE "last statement in do must be an expression"
    }
 | apoes
    {% withloc $ \l -> appsPE l (head $1) (tail $1) }

apoes :: { [PatOrExp] }
 : apoe
    { [$1] }
 | apoes apoe
    { $1 ++ [$2] }

apoe :: { PatOrExp }
 : var
    {% withloc $ \l -> varPE l $1 }
 | var '@' apoe
    { asPE $1 $3 }
 | gcon
    {% withloc $ \l -> conPE l $1 }
 | literal
    { $1 }
 | '(' poe ')'
    { $2 }
 | '(' poe ',' poes_commasep ')'
    {% withloc $ \l -> tuplePE l ($2 : $4) }
 | '[' poe '..' ']'
    {% withloc $ \l -> fromPE l $2 }
 | '[' poe '..' poe ']'
    {% withloc $ \l -> fromtoPE l $2 $4 }
 | '[' poe ',' poe '..' ']'
    {% withloc $ \l -> fromthenPE l $2 $4 }
 | '[' poe ',' poe '..' poe ']'
    {% withloc $ \l -> fromthentoPE l $2 $4 $6 }
 | '[' poe '|' guards ']'
    {% withloc $ \l -> lcompPE l $2 $4 }
 | '[' poe ']'
    {% withloc $ \l -> listPE l [$2] }
 | '[' poe ',' poes_commasep ']'
    {% withloc $ \l -> listPE l ($2 : $4) }
 | apoe '{' lopt(fbinds) '}'
    {% withloc $ \l ->  updatePE l $1 $3 }

guard :: { Guard }
 : poe '<-' poe {% do
     p <- toPat $1
     e <- toExp $3
     return (PatG p e)
   }
 | poe
    {% fmap BoolG (toExp $1) }
 | 'let' ldecls
    { LetG (lcoalesce $2) }

guards :: { [Guard] }
 : guard
    { [$1] }
 | guards ',' guard
    { $1 ++ [$3] }

literal :: { PatOrExp }
 : integer
    {% withloc $ \l -> integerPE l $1 }
 | char
    {% withloc $ \l -> charPE l $1 }
 | string
    {% withloc $ \l -> stringPE l $1 }

alts :: { [Alt] }
 : alt
    { [$1] }
 | alts ';' alt
    { $1 ++ [$3] }

alt :: { Alt }
 : poe '->' poe lopt(wdecls) {% do
    p <- toPat $1
    e <- toExp $3
    withloc $ \l -> simpleA l p e $4
  }
 | poe bodies lopt(wdecls) {% withlocM $ \l -> do
    p <- toPat $1
    return (Alt p (WBodies l $2 $3))
  }

bodies :: { [Body] }
 : body { [$1] }
 | bodies body { $1 ++ [$2] }

body :: { Body }
 : '|' guards '->' poe
    {% withlocM $ \l -> fmap (Body l $2) (toExp $4) }

stmts :: { [Stmt] }
 : stmt 
    { [$1] }
 | stmts ';' stmt
    { $1 ++ [$3] }

stmt :: { Stmt }
 : poe '<-' poe {% do
    p <- toPat $1
    e <- toExp $3
    return (BindS p e)
   }
 | poe 
    {% fmap NoBindS (toExp $1) }
 | 'let' '{' ldecls opt(';') '}'
    { LetS (lcoalesce $3) }

fbinds :: { [(Name, Exp)] }
 : fbind 
    { [$1] }
 | fbinds ',' fbind
    { $1 ++ [$3] }

fbind :: { (Name, Exp) }
 : var '=' poe
    {% fmap ((,) $1) (toExp $3) }

gcon :: { Name }
 : '(' ')'
    { unitN }
 | '[' ']'
    { name "[]" }
 | '(' commas ')'
    { name $ "(" ++ $2 ++ ")" }
 | qcon
    { $1 }

varid :: { Name }
 : varid_ { $1 }
 | 'qualified' { name "qualified" }
 | 'as' { name "as" }

var :: { Name }
 : varid
    { $1 }
 | '(' varsym ')'
    { $2 }
 | '(' varsym_op ')'
    { $2 }

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
    {% withloc $ \l -> varPE l $1 }
 | '`' varid '`'
    {% withloc $ \l -> varPE l $2 }
 | consym
    {% withloc $ \l -> conPE l $1 }

varsym_op :: { Name }
 : '+' { name "+" }
 | '-' { name "-" }
 | '*' { name "*" }
 | '$'  { name "$" }
 | '.'  { name "." }
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

qconid :: { Name }
 : conid
    { $1 }
 | qconid '.' conid
    { $1 `nappend` name "." `nappend` $3 }

commas :: { String }
 : ','
    { "," }
 | commas ','
    { ',':$1 }

tycls_commasep :: { [Name] }
 : conid 
    { [$1] }
 | tycls_commasep ',' conid
    { $1 ++ [$3] }

types_commasep :: { [Type] }
 : type
    { [$1] }
 | types_commasep ',' type
    { $1 ++ [$3] }

poes_commasep :: { [PatOrExp] }
 : poe
    { [$1] }
 | poes_commasep ',' poe
    { $1 ++ [$3] }

tyvar :: { TyVar }
 : varid
    { TyVar $1 UnknownK }
 | '#' varid
    { TyVar $2 NumK }

tyvars :: { [TyVar] }
 : tyvar
    { [$1] }
 | tyvars tyvar
    { $1 ++ [$2] }

tyvarnms :: { [Name] }
 : varid { [$1] }
 | tyvarnms varid { $1 ++ [$2] }

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
parse fp str = {-# SCC "Parse" #-} runParser smten_module fp str

} 

