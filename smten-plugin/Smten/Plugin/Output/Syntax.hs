
module Smten.Plugin.Output.Syntax (
    Name, LanguagePragma, TyVar,
    Module(..), Con(..), Type(..), Class, Dec(..), Val(..), Data(..),
    Method(..), Field(..), Exp(..), Alt(..), Pat(..), Literal(..), RecField(..),
    arrowT,
    tup2P, wildP,
    conE, tup2E,
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
data Val = Val Name (Maybe Type) Exp

data RecField = RecField Name Type

data Field = Field Name Exp

data Con = Con Name [Type]
         | RecC Name [RecField]

data Type = ConAppT Name [Type]
          | ForallT [TyVar] [Class] Type
          | VarT Name
          | AppT Type Type
          | NumT Integer

type Class = Type

data Exp =
   VarE Name
 | LitE Literal
 | AppE Exp Exp
 | LetE [Val] Exp 
 | LamE Name Exp
 | CaseE Exp [Alt]
 | ListE [Exp]
 | RecE Exp [Field]
 | SigE Exp Type

conE :: Name -> [Exp] -> Exp
conE nm xs = foldl AppE (VarE nm) xs

data Alt = Alt Pat Exp

data Pat = LitP Literal
         | ConP Name [Pat]
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

tup2P :: Pat -> Pat -> Pat
tup2P a b = ConP "(,)" [a, b]

tup2E :: Exp -> Exp -> Exp
tup2E a b = conE "(,)" [a, b]

wildP :: Pat
wildP = VarP "_"

