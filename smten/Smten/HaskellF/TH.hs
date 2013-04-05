
{-# LANGUAGE TemplateHaskell #-}

module Smten.HaskellF.TH (derive_SmtenHF, haskellf_Data) where

import Data.Maybe(fromMaybe)
import Data.Functor((<$>))

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Smten.Failable
import qualified Smten.Name as S
import qualified Smten.Dec as S
import Smten.HaskellF.HaskellF
import Smten.HaskellF.Compile

-- Input are the names of a generated symbolic data type and a haskell data
-- type. We expect the unqualified names to be the same.
-- ex: derive_smtenHF ''Maybe ''S.Maybe
derive_SmtenHF :: Name -> Name -> Q [Dec]
derive_SmtenHF a b = do
  TyConI (DataD _ _ vars cs _) <- reify a
  let vA = [VarT (mkName $ 'a' : show i) | i <- [1..length vars]]
      vB = [VarT (mkName $ 'b' : show i) | i <- [1..length vars]]
      ctx = [ClassP ''SmtenHF [a, b] | (a, b) <- zip vA vB]
      ta = foldl AppT (ConT a) vA
      tb = foldl AppT (ConT b) vB
      ty = foldl AppT (ConT ''SmtenHF) [ta, tb]
      moda = fromMaybe "" ((++ ".") <$> nameModule a)
      modb = fromMaybe "" ((++ ".") <$> nameModule b)
      smtens = derive_smtenHF moda modb vars cs
      de_smtens = derive_de_smtenHF moda modb vars cs
  return [InstanceD ctx ty (concat [smtens, de_smtens])]

derive_smtenHF :: String -> String -> [TyVarBndr] -> [Con] -> [Dec]
derive_smtenHF c f vars cs =
  let -- Each data constructor has it's own clause in the smtenHF function.
      -- A constructor of the form:
      --    Bar Sludge a
      -- Maps to the clause:
      --    *** <c>.Bar a b = <f>.Bar (smtenHF a) (smtenHF b)
      mkcon :: Con -> Clause
      mkcon (NormalC cnm ts) =   
        let args = [mkName ('x' : show i) | i <- [1..length ts]]
            pat = ConP cnm (map VarP args)
            argsS = [AppE (VarE 'smtenHF) (VarE a) | a <- args]
            body = foldl AppE (ConE (mkName (f ++ nameBase cnm))) argsS
        in Clause [pat] (NormalB body) []
      mkcon (RecC nm vsts) = mkcon (NormalC nm (map (\(_, s, t) -> (s, t)) vsts))
      mkcon (InfixC ta n tb) = mkcon $ NormalC n [ta, tb]

      dec = FunD 'smtenHF (map mkcon cs)
  in [dec]

-- TODO: derive_de_smtenHF
-- For now we'll just fall back on the less efficient default.
derive_de_smtenHF :: String -> String -> [TyVarBndr] -> [Con] -> [Dec]
derive_de_smtenHF c f vars cs = []


haskellf_Data :: S.Name -> [S.TyVar] -> [S.Con] -> Q [Dec]
haskellf_Data n tyvars constrs = attemptM $ hfData n tyvars constrs

