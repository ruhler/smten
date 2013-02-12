
{-# LANGUAGE TemplateHaskell #-}

module Smten.HaskellF.TH (derive_SmtenS) where

import Data.Maybe(fromMaybe)
import Data.Functor((<$>))

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Smten.HaskellF.Symbolic

-- Input are the names of a generated symbolic data type and a haskell data
-- type. We expect the unqualified names to be the same.
-- ex: derive_smtenS ''Maybe ''S.Maybe
derive_SmtenS :: Name -> Name -> Q [Dec]
derive_SmtenS a b = do
  TyConI (DataD _ _ vars cs _) <- reify a
  let vA = [VarT (mkName $ 'a' : show i) | i <- [1..length vars]]
      vB = [VarT (mkName $ 'b' : show i) | i <- [1..length vars]]
      ctx = [ClassP ''SmtenS [a, b] | (a, b) <- zip vA vB]
      ta = foldl AppT (ConT a) vA
      tb = foldl AppT (ConT b) vB
      ty = foldl AppT (ConT ''SmtenS) [ta, tb]
      moda = fromMaybe "" ((++ ".") <$> nameModule a)
      modb = fromMaybe "" ((++ ".") <$> nameModule b)
      smtens = derive_smtenS moda modb vars cs
      de_smtens = derive_de_smtenS moda modb vars cs
  return [InstanceD ctx ty (concat [smtens, de_smtens])]

derive_smtenS :: String -> String -> [TyVarBndr] -> [Con] -> [Dec]
derive_smtenS c f vars cs =
  let -- Each data constructor has it's own clause in the smtenS function.
      -- A constructor of the form:
      --    Bar Sludge a
      -- Maps to the clause:
      --    *** <c>.Bar a b = <f>.Bar (smtenS a) (smtenS b)
      mkcon :: Con -> Clause
      mkcon (NormalC cnm ts) =   
        let args = [mkName ('x' : show i) | i <- [1..length ts]]
            pat = ConP cnm (map VarP args)
            argsS = [AppE (VarE 'smtenS) (VarE a) | a <- args]
            body = foldl AppE (ConE (mkName (f ++ nameBase cnm))) argsS
        in Clause [pat] (NormalB body) []
      mkcon (RecC nm vsts) = mkcon (NormalC nm (map (\(_, s, t) -> (s, t)) vsts))
      mkcon (InfixC ta n tb) = mkcon $ NormalC n [ta, tb]

      dec = FunD 'smtenS (map mkcon cs)
  in [dec]

-- TODO: derive_de_smtenS
-- For now we'll just fall back on the less efficient default.
derive_de_smtenS :: String -> String -> [TyVarBndr] -> [Con] -> [Dec]
derive_de_smtenS c f vars cs = []

