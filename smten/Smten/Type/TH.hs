
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Template haskell utilities for Types
module Smten.Type.TH (derive_SmtenT) where

import Language.Haskell.TH

import qualified Smten.Name as S
import qualified Smten.Type.Sugar as S

derive_SmtenT :: String -> Name -> Q [Dec]
derive_SmtenT mod nm = do
  reified <- reify nm
  let vars = 
       case reified of
          TyConI (DataD _ _ vars _ _) -> vars
          TyConI (NewtypeD _ _ vars _ _) -> vars
          _ -> error $ "derive_SmtenT: " ++ show reified
      vn = if null vars then "" else show (length vars)
      ty = AppT (ConT (mkName $ "SmtenT" ++ vn)) (ConT nm)
      body = AppE (VarE 'S.conT) (AppE (VarE 'S.name) (LitE (StringL (mod ++ "." ++ nameBase nm))))
      dec = FunD (mkName $ "smtenT" ++ vn) [Clause [WildP] (NormalB body) []]
  return [InstanceD [] ty [dec]]

