
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Template Haskell utilities for enoch.
module Seri.Enoch.EnochTH ( 
    derive_SeriableT, derive_SeriableE, loadenvth
 ) where 

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Seri.Enoch.Enoch
import qualified Seri.Lambda as S
import qualified Seri.Enoch.Prelude as S

derive_SeriableT :: Name -> Q [Dec]
derive_SeriableT nm = do
  TyConI (DataD _ _ vars _ _) <- reify nm
  let vn = if null vars then "" else show (length vars)
  let ty = AppT (ConT (mkName $ "SeriableT" ++ vn)) (ConT nm)
  let body = AppE (ConE 'S.ConT) (AppE (VarE 'S.name) (LitE (StringL (nameBase nm))))
  let dec = FunD (mkName $ "serit" ++ vn) [Clause [WildP] (NormalB body) []]
  return [InstanceD [] ty [dec]]

derive_SeriableE :: Name -> Q [Dec]
derive_SeriableE nm = do
  TyConI (DataD _ _ vars cs _) <- reify nm
  let ctx = [ClassP ''SeriableE [VarT v] | PlainTV v <- vars]
  let ty = AppT (ConT ''SeriableE) (foldl AppT (ConT nm) [VarT v | PlainTV v <- vars])
  return [InstanceD ctx ty (concat [derive_pack nm vars cs, derive_unpack nm vars cs])]
  
  
-- Given the name of a type constructor Foo, produces the pack function:
--  pack :: Foo -> TExp Foo
derive_pack :: Name -> [TyVarBndr] -> [Con] -> [Dec]
derive_pack nm vars cs =
  let -- Each data constructor has it's own clause in the pack function.
      -- A constructor of the form:
      --    Bar Sludge a
      -- Maps to the clause:
      --    *** (Bar a b) =
      --      let con :: TExp (Sludge -> a -> Foo a)
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
            consig = SigD connm (AppT (ConT ''TExp) (arrowsT (map snd ts ++ [foldl AppT (ConT nm) [VarT v | PlainTV v <- vars]])))
            conbody = ValD (VarP connm) (NormalB $ AppE (VarE 'S.conE) (LitE (StringL (nameBase cnm)))) []
            con_ = ValD (ConP 'TExp [VarP con_nm]) (NormalB $ VarE connm) []
            decls = [consig, conbody, con_]
            exp = AppE (ConE 'TExp) (AppE (VarE 'S.appsE) (ListE $ VarE con_nm : map (\a -> AppE (VarE 'packE) (VarE a)) args))
            body = LetE decls exp
        in Clause [pat] (NormalB body) []
      mkcon (RecC nm vsts) = mkcon (NormalC nm (map (\(_, s, t) -> (s, t)) vsts))
      mkcon (InfixC ta n tb) = mkcon $ NormalC n [ta, tb]

      dec = FunD 'pack (map mkcon cs)
  in [dec]
    
-- Given the name of a type constructor Foo, produces the unpack function:
--  unpack :: TExp Foo -> Maybe Foo
derive_unpack :: Name -> [TyVarBndr] -> [Con] -> [Dec]
derive_unpack nm vars cs =
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
      mkcon (InfixC ta n tb) = mkcon $ NormalC n [ta, tb]

      defaultmatch :: Match
      defaultmatch = Match WildP (NormalB (ConE 'Nothing)) []

      body = CaseE (AppE (VarE 'S.unappsE) (VarE (mkName "e"))) (map mkcon cs ++ [defaultmatch])
      clause = Clause [ConP 'TExp [VarP (mkName "e")]] (NormalB body) []
      dec = FunD 'unpack [clause]
  in [dec]


-- Load a seri environment at compile time.
-- Performs type checking on the environment statically.
-- Returns an expression of type Env.
--
-- It takes the paths as IO computations to make it work more naturally with
-- the seridir function.
loadenvth :: [IO FilePath] -> IO FilePath -> Q Exp
loadenvth iopath iofin = do
    env <- runIO $ do
        path <- sequence iopath
        fin <- iofin
        S.loadenv path fin
    let decls = S.getDecls env
    [| S.mkEnv decls |]

instance Lift S.Dec where
    lift (S.ValD t e) = [| S.ValD t e |]
    lift (S.DataD n vars cs) = [| S.DataD n vars cs |]
    lift (S.ClassD n vars ts) = [| S.ClassD n vars ts |]
    lift (S.InstD ctx cls ms) = [| S.InstD ctx cls ms |]
    lift (S.PrimD ts) = [| S.PrimD ts |]

instance Lift S.TopSig where
    lift (S.TopSig n c t) = [| S.TopSig n c t |]

instance Lift S.Type where
    lift (S.ConT n) = [| S.ConT n |]
    lift (S.AppT a b) = [| S.AppT a b |]
    lift (S.VarT n) = [| S.VarT n |]
    lift (S.NumT n) = [| S.NumT n |]
    lift (S.UnknownT) = [| S.UnknownT |]

instance Lift S.NType where
    lift (S.ConNT i) = [| S.ConNT i |]
    lift (S.VarNT n) = [| S.VarNT n |]
    lift (S.AppNT op a b) = [| S.AppNT op a b |]

instance Lift S.Pat where
    lift (S.ConP t n ps) = [| S.ConP t n ps |]
    lift (S.VarP s) = [| S.VarP s |]
    lift (S.LitP l) = [| S.LitP l |]
    lift (S.WildP t) = [| S.WildP t |]

instance Lift S.Lit where
    lift (S.IntegerL i) = [| S.IntegerL i |]
    lift (S.CharL c) = [| S.CharL c |]

instance Lift S.Sig where
    lift (S.Sig n t) = [| S.Sig n t |]

instance Lift S.Match where
    lift (S.Match ps e) = [| S.Match ps e |]
    

instance Lift S.Exp where
    lift (S.LitE l) = [| S.LitE l |]
    lift (S.ConE s) = [| S.ConE s |]
    lift (S.VarE s) = [| S.VarE s |]
    lift (S.LaceE ms) = [| S.LaceE ms |]
    lift (S.AppE f xs) = [| S.AppE f xs |]

instance Lift S.Name where
    lift s = let str = S.unname s in [| S.name str |]

instance Lift S.TyVar where
    lift (S.NormalTV n) = [| S.NormalTV n |]
    lift (S.NumericTV n) = [| S.NumericTV n |]
    
instance Lift S.Con where
    lift (S.Con n ts) = [| S.Con n ts |]

instance Lift S.Class where
    lift (S.Class n ts) = [| S.Class n ts |]

instance Lift S.Method where
    lift (S.Method n e) = [| S.Method n e |]

