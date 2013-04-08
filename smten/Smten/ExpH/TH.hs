
{-# LANGUAGE TemplateHaskell #-}

-- | Template haskell utilities for working with ExpH.
module Smten.ExpH.TH (
    derive_SmtenEH
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Smten.Name as S
import Smten.Sig
import qualified Smten.Type as S
import Smten.ExpH.ExpH
import Smten.ExpH.Sugar
import Smten.ExpH.SmtenEH

smtenEH_helper :: (S.SmtenT a) => String -> a -> [S.Type] -> [ExpH] -> ExpH
smtenEH_helper nm ty tys xs = 
  let t = S.arrowsT (tys ++ [S.smtenT ty])
  in appsEH (conEH (Sig (S.name nm) t)) xs

derive_SmtenEH :: String -> Name -> Q [Dec]
derive_SmtenEH mod nm = do
  TyConI (DataD _ _ vars cs _) <- reify nm
  let ctx = [ClassP ''SmtenEH [VarT v] | PlainTV v <- vars]
  let ty = AppT (ConT ''SmtenEH) (foldl AppT (ConT nm) [VarT v | PlainTV v <- vars])
  return [InstanceD ctx ty (concat [derive_smtenEH mod nm vars cs, derive_de_smtenEH mod nm vars cs])]
  
  
-- Given the name of a type constructor Foo, produces the smtenEH function:
--  smtenEH :: Foo -> ExpH
derive_smtenEH :: String -> Name -> [TyVarBndr] -> [Con] -> [Dec]
derive_smtenEH mod nm vars cs =
  let -- Each data constructor has it's own clause in the smtenEH function.
      -- A constructor of the form:
      --    Bar Sludge a
      -- Maps to the clause:
      --    *** exp@(Bar a b) = smtenEH_helper "Bar" exp [smtenT a, smtenT b] [smtenEH a, smtenEH b]
      mkcon :: Con -> Clause
      mkcon (NormalC cnm ts) =   
        let args = [mkName ('x' : show i) | i <- [1..length ts]]
            expvar = mkName "exp"
            pat = AsP expvar (ConP cnm (map VarP args))
            elist = ListE $ map (\a -> AppE (VarE 'smtenEH) (VarE a)) args
            tlist = ListE $ map (\a -> AppE (VarE 'S.smtenT) (VarE a)) args
            body = foldl1 AppE [
                      VarE 'smtenEH_helper,
                      LitE (StringL (mod ++ "." ++ nameBase cnm)),
                      VarE expvar,
                      tlist,
                      elist
                    ]
        in Clause [pat] (NormalB body) []
      mkcon (RecC nm vsts) = mkcon (NormalC nm (map (\(_, s, t) -> (s, t)) vsts))
      mkcon (InfixC ta n tb) = mkcon $ NormalC n [ta, tb]

      dec = FunD 'smtenEH (map mkcon cs)
  in [dec]
    
-- Given the name of a type constructor Foo, produces the de_smtenEH function:
--  de_smtenEH :: ExpH -> Maybe Foo
derive_de_smtenEH :: String -> Name -> [TyVarBndr] -> [Con] -> [Dec]
derive_de_smtenEH mod nm vars cs =
  let -- Each data constructor has it's own match in the unpack case expr.
      -- A constructor of the form:
      --    Bar Sludge Fudge
      -- Maps to the clause:
      --    [x] | Just [x1, x2] <- de_kconEH (name "Bar") x -> do
      --      x1' <- de_smtenEH x1
      --      x2' <- de_smtenEH x2
      --      return (Bar x1' x2')
      mkcon :: Con -> Match
      mkcon (NormalC cnm ts) =   
        let args = [mkName ("x" ++ show i) | i <- take (length ts) [1..]]
            args' = [mkName ("x" ++ show i ++ "'") | i <- take (length ts) [1..]]
            pat = VarP (mkName "x")
            argpat = ConP 'Just [ListP (map VarP args)]
            guard = PatG [BindS argpat (AppE (AppE (VarE (mkName "de_kconEH")) (AppE (VarE 'S.name) (LitE (StringL (mod ++ "." ++ nameBase cnm))))) (VarE (mkName "x")))]
            stmts = [BindS (VarP x') (AppE (VarE 'de_smtenEH) (VarE x)) | (x, x') <- zip args args']
            body = DoE $ stmts ++ [NoBindS (AppE (VarE 'return) (foldl AppE (ConE cnm) (map VarE args')))]
        in Match pat (GuardedB [(guard, body)]) []
      mkcon (RecC nm vsts) = mkcon (NormalC nm (map (\(_, s, t) -> (s, t)) vsts))
      mkcon (InfixC ta n tb) = mkcon $ NormalC n [ta, tb]

      defaultmatch :: Match
      defaultmatch = Match WildP (NormalB (ConE 'Nothing)) []

      body = CaseE (VarE (mkName "e")) (map mkcon cs ++ [defaultmatch])
      clause = Clause [VarP (mkName "e")] (NormalB body) []
      dec = FunD 'de_smtenEH [clause]
  in [dec]

