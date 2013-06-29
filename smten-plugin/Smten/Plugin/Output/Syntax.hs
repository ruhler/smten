
module Smten.Plugin.Output.Syntax (
    Name, LanguagePragma, TyVar,
    Module(..), DataD(..), ValD(..), Con(..), Type(..), Exp(..), Alt(..),
    Pat(..), Literal(..),
    ) where

type Name = String
type LanguagePragma = String
type TyVar = String

data Module = Module {
  mod_langs :: [LanguagePragma],
  mod_name :: Name,
  mod_imports :: [Name],
  mod_datas :: [DataD],
  mod_vals :: [ValD]
}

data DataD = DataD Name [TyVar] [Con]

data ValD = ValD Name Type Exp

data Con = Con Name [Type]

data Type = ConAppT Name [Type]
          | ForallT [TyVar] Type
          | VarT Name
          | AppT Type Type

data Exp =
   VarE Name
 | LitE Literal
 | AppE Exp Exp
 | LetE [ValD] Exp 
 | LamE Name Exp
 | CaseE Exp [Alt]

data Alt = Alt Pat Exp

data Pat = LitP Literal
         | ConP Name [Name]
         | VarP Name
         | AsP Name Pat

data Literal = 
    StringL String
  | CharL Char
  | IntL Integer
  | WordL Integer
  | IntegerL Integer

