
{-# LANGUAGE TemplateHaskell #-}

module Seri.HaskellF.TH (derive_SeriS) where

import Data.Maybe(fromMaybe)
import Data.Functor((<$>))

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Seri.HaskellF.Symbolic

-- Input are the names of a generated symbolic data type and a haskell data
-- type. We expect the unqualified names to be the same.
-- ex: derive_seriS ''Maybe ''S.Maybe
derive_SeriS :: Name -> Name -> Q [Dec]
derive_SeriS a b = do
  TyConI (DataD _ _ vars cs _) <- reify a
  let vA = [VarT (mkName $ 'a' : show i) | i <- [1..length vars]]
      vB = [VarT (mkName $ 'b' : show i) | i <- [1..length vars]]
      ctx = [ClassP ''SeriS [a, b] | (a, b) <- zip vA vB]
      ta = foldl AppT (ConT a) vA
      tb = foldl AppT (ConT b) vB
      ty = foldl AppT (ConT ''SeriS) [ta, tb]
      moda = fromMaybe "" ((++ ".") <$> nameModule a)
      modb = fromMaybe "" ((++ ".") <$> nameModule b)
      seris = derive_seriS moda modb vars cs
      de_seris = derive_de_seriS moda modb vars cs
  return [InstanceD ctx ty (concat [seris, de_seris])]

derive_seriS :: String -> String -> [TyVarBndr] -> [Con] -> [Dec]
derive_seriS c f vars cs =
  let -- Each data constructor has it's own clause in the seriS function.
      -- A constructor of the form:
      --    Bar Sludge a
      -- Maps to the clause:
      --    *** <c>.Bar a b = <f>.Bar (seriS a) (seriS b)
      mkcon :: Con -> Clause
      mkcon (NormalC cnm ts) =   
        let args = [mkName ('x' : show i) | i <- [1..length ts]]
            pat = ConP cnm (map VarP args)
            argsS = [AppE (VarE 'seriS) (VarE a) | a <- args]
            body = foldl AppE (ConE (mkName (f ++ nameBase cnm))) argsS
        in Clause [pat] (NormalB body) []
      mkcon (RecC nm vsts) = mkcon (NormalC nm (map (\(_, s, t) -> (s, t)) vsts))
      mkcon (InfixC ta n tb) = mkcon $ NormalC n [ta, tb]

      dec = FunD 'seriS (map mkcon cs)
  in [dec]

-- TODO: derive_de_seriS
-- For now we'll just fall back on the less efficient default.
derive_de_seriS :: String -> String -> [TyVarBndr] -> [Con] -> [Dec]
derive_de_seriS c f vars cs = []

