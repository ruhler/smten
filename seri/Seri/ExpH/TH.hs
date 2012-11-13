
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

unappsEH :: ExpH -> [ExpH]
unappsEH e =
  let (f, xs) = de_appsEH e
  in f:xs

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
        let args = [mkName [c] | c <- take (length ts) "abcdefghijklmnopqrstuvwxyz"]
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
      --    [ConEH (Sig n _), a, b] | n == name "Bar" -> do
      --      a' <- de_seriEH a
      --      b' <- de_seriEH b
      --      return (Bar a' b')
      mkcon :: Con -> Match
      mkcon (NormalC cnm ts) =   
        let args = [mkName [c] | c <- take (length ts) "abcdefghijklmnopqrstuvwxyz"]
            args' = [mkName (c:'\'':[]) | c <- take (length ts) "abcdefghijklmnopqrstuvwxyz"]
            pat = ListP (ConP 'ConEH [ConP 'Sig [VarP (mkName "nm"), WildP]] : map VarP args)
            guard = NormalG $ AppE (AppE (VarE '(==)) (VarE (mkName "nm"))) (AppE (VarE 'S.name) (LitE (StringL (nameBase cnm))))
            stmts = [BindS (VarP x') (AppE (VarE 'de_seriEH) (VarE x)) | (x, x') <- zip args args']
            body = DoE $ stmts ++ [NoBindS (AppE (VarE 'return) (foldl AppE (ConE cnm) (map VarE args')))]
        in Match pat (GuardedB [(guard, body)]) []
      mkcon (RecC nm vsts) = mkcon (NormalC nm (map (\(_, s, t) -> (s, t)) vsts))
      mkcon (InfixC ta n tb) = mkcon $ NormalC n [ta, tb]

      defaultmatch :: Match
      defaultmatch = Match WildP (NormalB (ConE 'Nothing)) []

      body = CaseE (AppE (VarE 'unappsEH) (VarE (mkName "e"))) (map mkcon cs ++ [defaultmatch])
      clause = Clause [VarP (mkName "e")] (NormalB body) []
      dec = FunD 'de_seriEH [clause]
  in [dec]

