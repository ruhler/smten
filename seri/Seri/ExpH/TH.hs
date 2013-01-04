
{-# LANGUAGE TemplateHaskell #-}

-- | Template haskell utilities for working with ExpH.
module Seri.ExpH.TH (
    derive_SeriEH
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Seri.Name as S
import Seri.Sig
import qualified Seri.Type as S
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar
import Seri.ExpH.SeriEH

seriEH_helper :: (S.SeriT a) => String -> a -> [S.Type] -> [ExpH] -> ExpH
seriEH_helper nm ty tys xs = 
  let t = S.arrowsT (tys ++ [S.seriT ty])
  in appsEH (conEH (Sig (S.name nm) t)) xs

derive_SeriEH :: Name -> Q [Dec]
derive_SeriEH nm = do
  TyConI (DataD _ _ vars cs _) <- reify nm
  let ctx = [ClassP ''SeriEH [VarT v] | PlainTV v <- vars]
  let ty = AppT (ConT ''SeriEH) (foldl AppT (ConT nm) [VarT v | PlainTV v <- vars])
  return [InstanceD ctx ty (concat [derive_seriEH nm vars cs, derive_de_seriEH nm vars cs])]
  
  
-- Given the name of a type constructor Foo, produces the seriEH function:
--  seriEH :: Foo -> ExpH
derive_seriEH :: Name -> [TyVarBndr] -> [Con] -> [Dec]
derive_seriEH nm vars cs =
  let -- Each data constructor has it's own clause in the seriEH function.
      -- A constructor of the form:
      --    Bar Sludge a
      -- Maps to the clause:
      --    *** exp@(Bar a b) = seriEH_helper "Bar" exp [seriT a, seriT b] [seriEH a, seriEH b]
      mkcon :: Con -> Clause
      mkcon (NormalC cnm ts) =   
        let args = [mkName ('x' : show i) | i <- [1..length ts]]
            expvar = mkName "exp"
            pat = AsP expvar (ConP cnm (map VarP args))
            elist = ListE $ map (\a -> AppE (VarE 'seriEH) (VarE a)) args
            tlist = ListE $ map (\a -> AppE (VarE 'S.seriT) (VarE a)) args
            body = foldl1 AppE [
                      VarE 'seriEH_helper,
                      LitE (StringL (nameBase cnm)),
                      VarE expvar,
                      tlist,
                      elist
                    ]
        in Clause [pat] (NormalB body) []
      mkcon (RecC nm vsts) = mkcon (NormalC nm (map (\(_, s, t) -> (s, t)) vsts))
      mkcon (InfixC ta n tb) = mkcon $ NormalC n [ta, tb]

      dec = FunD 'seriEH (map mkcon cs)
  in [dec]
    
-- Given the name of a type constructor Foo, produces the de_seriEH function:
--  de_seriEH :: ExpH -> Maybe Foo
derive_de_seriEH :: Name -> [TyVarBndr] -> [Con] -> [Dec]
derive_de_seriEH nm vars cs =
  let -- Each data constructor has it's own match in the unpack case expr.
      -- A constructor of the form:
      --    Bar Sludge Fudge
      -- Maps to the clause:
      --    [x] | Just [x1, x2] <- de_kconEH (name "Bar") x -> do
      --      x1' <- de_seriEH x1
      --      x2' <- de_seriEH x2
      --      return (Bar x1' x2')
      mkcon :: Con -> Match
      mkcon (NormalC cnm ts) =   
        let args = [mkName ("x" ++ show i) | i <- take (length ts) [1..]]
            args' = [mkName ("x" ++ show i ++ "'") | i <- take (length ts) [1..]]
            pat = VarP (mkName "x")
            argpat = ConP 'Just [ListP (map VarP args)]
            guard = PatG [BindS argpat (AppE (AppE (VarE (mkName "de_kconEH")) (AppE (VarE 'S.name) (LitE (StringL (nameBase cnm))))) (VarE (mkName "x")))]
            stmts = [BindS (VarP x') (AppE (VarE 'de_seriEH) (VarE x)) | (x, x') <- zip args args']
            body = DoE $ stmts ++ [NoBindS (AppE (VarE 'return) (foldl AppE (ConE cnm) (map VarE args')))]
        in Match pat (GuardedB [(guard, body)]) []
      mkcon (RecC nm vsts) = mkcon (NormalC nm (map (\(_, s, t) -> (s, t)) vsts))
      mkcon (InfixC ta n tb) = mkcon $ NormalC n [ta, tb]

      defaultmatch :: Match
      defaultmatch = Match WildP (NormalB (ConE 'Nothing)) []

      body = CaseE (VarE (mkName "e")) (map mkcon cs ++ [defaultmatch])
      clause = Clause [VarP (mkName "e")] (NormalB body) []
      dec = FunD 'de_seriEH [clause]
  in [dec]

