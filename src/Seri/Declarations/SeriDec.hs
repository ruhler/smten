
{-# LANGUAGE TemplateHaskell #-}

module Seri.Declarations.SeriDec (
    SeriDec(..), seridec, withdecs,
    ) where

import Language.Haskell.TH

import Seri.THUtils
import qualified Seri.IR as S
import Seri.Typed(enved)

class SeriDec a where
    dec :: a -> S.Dec

-- Produce declarations for:
--  data SeriDec<Name> = <Name>
--  instance SeriDec <Name> where
--      dec _ = <Exp>
seridec :: Name -> Exp -> [Dec]
seridec nm body = 
  let n = prefixed "SeriDec" nm
      data_D = DataD [] n [] [NormalC n []] []
      impl_D = FunD 'dec [Clause [WildP] (NormalB body) []]
      inst_D = InstanceD [] (AppT (ConT ''SeriDec) (ConT n)) [impl_D]
  in [data_D, inst_D]

-- Given an expression (Typed Exp a), -- return an Env expression
-- (Typed (Env Exp) a) with all current seri declarations added.
withdecs :: Exp -> Q Exp
withdecs e = do
    ClassI _ insts <- reify ''SeriDec
    let tys = map (head . ci_tys) insts
    let decs = map (\(ConT n) -> apply 'dec [ConE $ mkName (nameBase n)]) tys
    return $ apply 'enved [e, ListE decs]

