
module Smten.Plugin.Output.Syntax (
    Name, LanguagePragma, TyVar,
    Module(..), Con(..), Type(..), Class, Dec(..), Val(..), Data(..),
    Method(..), Exp(..), Alt(..), Pat(..), Literal(..), RecField(..),
    arrowT,
    conE,
    ) where

type Name = String
type LanguagePragma = String
type TyVar = String

data Module = Module {
  mod_langs :: [LanguagePragma],
  mod_name :: Name,
  mod_imports :: [Name],
  mod_decs :: [Dec]
}

data Dec = DataD Data
         | ValD Val
         | InstD [Class] Type [Method]

data Method = Method Name Exp

data Data = Data Name [TyVar] [Con]
data Val = Val Name Type Exp

data RecField = RecField Name Type

data Con = Con Name [Type]
         | RecC Name [RecField]

data Type = ConAppT Name [Type]
          | ForallT [TyVar] [Class] Type
          | VarT Name
          | AppT Type Type

type Class = Type

data Exp =
   VarE Name
 | LitE Literal
 | AppE Exp Exp
 | LetE [Val] Exp 
 | LamE Name Exp
 | CaseE Exp [Alt]
 | ListE [Exp]

conE :: Name -> [Exp] -> Exp
conE nm xs = foldl AppE (VarE nm) xs

data Alt = Alt Pat Exp

data Pat = LitP Literal
         | ConP Name [Name]
         | RecP Name            -- Foo {}
         | VarP Name
         | AsP Name Pat

data Literal = 
    StringL String
  | CharL Char
  | IntL Integer
  | WordL Integer
  | IntegerL Integer

arrowT :: Type -> Type -> Type
arrowT a b = ConAppT "(->)" [a, b]

