
{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell utilities for enoch.
module Seri.Enoch.EnochTH ( 
    derive_SeriableT, derive_SeriableE,
 ) where 

import Language.Haskell.TH
import Seri.Enoch.Enoch
import qualified Seri.Lambda as S
import qualified Seri.Enoch.Prelude as S

derive_SeriableT :: Name -> Q [Dec]
derive_SeriableT nm = do
  -- TODO: support data types with type variables
  TyConI (DataD _ _ [] _ _) <- reify nm
  let ty = (AppT (ConT ''SeriableT) (ConT nm))
  let body = AppE (ConE 'S.ConT) (AppE (VarE 'S.name) (LitE (StringL (nameBase nm))))
  let dec = FunD 'serit [Clause [WildP] (NormalB body) []]
  return [InstanceD [] ty [dec]]

derive_SeriableE :: Name -> Q [Dec]
derive_SeriableE nm = do
  -- TODO: support data types with type variables
  TyConI (DataD _ _ [] cs _) <- reify nm
  let ty = (AppT (ConT ''SeriableE) (ConT nm))
  return [InstanceD [] ty (concat [derive_pack nm cs, derive_unpack nm cs])]
  
  
-- Given the name of a type constructor Foo, produces the pack function:
--  pack :: Foo -> TExp Foo
derive_pack :: Name -> [Con] -> [Dec]
derive_pack nm cs =
  let -- Each data constructor has it's own clause in the pack function.
      -- A constructor of the form:
      --    Bar Sludge Fudge
      -- Maps to the clause:
      --    *** (Bar a b) =
      --      let con :: TExp (Sludge -> Fudge -> Foo)
      --          con = conE "Bar"
      --          
      --          TExp con_ = con
      --      in TExp (appsE [con_, pack a, pack b])
      mkcon :: Con -> Clause
      mkcon (NormalC cnm ts) =   
        let arrowsT [x] = x
            arrowsT (x:xs) = AppT (AppT ArrowT x) (arrowsT xs)
            args = [mkName [c] | c <- take (length ts) "abcdefghijklmnopqrstuvwxyz"]
            pat = ConP cnm (map VarP args)
            connm = mkName "con"
            con_nm = mkName "con_"
            consig = SigD connm (AppT (ConT ''TExp) (arrowsT (map snd ts ++ [ConT nm])))
            conbody = ValD (VarP connm) (NormalB $ AppE (VarE 'S.conE) (LitE (StringL (nameBase cnm)))) []
            con_ = ValD (ConP 'TExp [VarP con_nm]) (NormalB $ VarE connm) []
            decls = [consig, conbody, con_]
            exp = AppE (ConE 'TExp) (AppE (VarE 'S.appsE) (ListE $ VarE con_nm : map (\a -> AppE (VarE 'packE) (VarE a)) args))
            body = LetE decls exp
        in Clause [pat] (NormalB body) []
      mkcon (RecC nm vsts) = mkcon (NormalC nm (map (\(_, s, t) -> (s, t)) vsts))

      dec = FunD 'pack (map mkcon cs)
  in [dec]
    
-- Given the name of a type constructor Foo, produces the unpack function:
--  unpack :: TExp Foo -> Maybe Foo
derive_unpack :: Name -> [Con] -> [Dec]
derive_unpack nm cs =
  let -- Each data constructor has it's own match in the unpack case expr.
      -- A constructor of the form:
      --    Bar Sludge Fudge
      -- Maps to the clause:
      --    [ConE (Sig n _), a, b] | n == name "Bar" -> do
      --      a' <- unpack (TExp a)
      --      b' <- unpack (TExp b)
      --      return (Bar a' b')
      mkcon :: Con -> Match
      mkcon (NormalC cnm ts) =   
        let args = [mkName [c] | c <- take (length ts) "abcdefghijklmnopqrstuvwxyz"]
            args' = [mkName (c:'\'':[]) | c <- take (length ts) "abcdefghijklmnopqrstuvwxyz"]
            pat = ListP (ConP 'S.ConE [ConP 'S.Sig [VarP (mkName "nm"), WildP]] : map VarP args)
            guard = NormalG $ AppE (AppE (VarE '(==)) (VarE (mkName "nm"))) (AppE (VarE 'S.name) (LitE (StringL (nameBase cnm))))
            stmts = [BindS (VarP x') (AppE (VarE 'unpack) (AppE (ConE 'TExp) (VarE x))) | (x, x') <- zip args args']
            body = DoE $ stmts ++ [NoBindS (AppE (VarE 'return) (foldl AppE (ConE cnm) (map VarE args')))]
        in Match pat (GuardedB [(guard, body)]) []
      mkcon (RecC nm vsts) = mkcon (NormalC nm (map (\(_, s, t) -> (s, t)) vsts))

      defaultmatch :: Match
      defaultmatch = Match WildP (NormalB (ConE 'Nothing)) []

      body = CaseE (AppE (VarE 'S.unappsE) (VarE (mkName "e"))) (map mkcon cs ++ [defaultmatch])
      clause = Clause [ConP 'TExp [VarP (mkName "e")]] (NormalB body) []
      dec = FunD 'unpack [clause]
  in [dec]

