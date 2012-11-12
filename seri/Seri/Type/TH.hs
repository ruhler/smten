
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Template haskell utilities for Types
module Seri.Type.TH (derive_SeriT) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Seri.Name as S
import qualified Seri.Type.Type as S
import Seri.Type.SeriT

derive_SeriT :: Name -> Q [Dec]
derive_SeriT nm = do
  TyConI (DataD _ _ vars _ _) <- reify nm
  let vn = if null vars then "" else show (length vars)
  let ty = AppT (ConT (mkName $ "SeriT" ++ vn)) (ConT nm)
  let body = AppE (ConE 'S.ConT) (AppE (VarE 'S.name) (LitE (StringL (nameBase nm))))
  let dec = FunD (mkName $ "seriT" ++ vn) [Clause [WildP] (NormalB body) []]
  return [InstanceD [] ty [dec]]

