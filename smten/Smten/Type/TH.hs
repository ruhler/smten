
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Template haskell utilities for Types
module Smten.Type.TH (derive_SmtenT) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Smten.Name as S
import qualified Smten.Type.Type as S
import Smten.Type.SmtenT

derive_SmtenT :: Name -> Q [Dec]
derive_SmtenT nm = do
  TyConI (DataD _ _ vars _ _) <- reify nm
  let vn = if null vars then "" else show (length vars)
  let ty = AppT (ConT (mkName $ "SmtenT" ++ vn)) (ConT nm)
  let body = AppE (ConE 'S.ConT) (AppE (VarE 'S.name) (LitE (StringL (nameBase nm))))
  let dec = FunD (mkName $ "smtenT" ++ vn) [Clause [WildP] (NormalB body) []]
  return [InstanceD [] ty [dec]]

