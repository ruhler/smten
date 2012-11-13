
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell utilities for seri.
module Seri.TH ( 
    loadenvth
 ) where 

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Seri.Type as S
import qualified Seri.Name as S
import qualified Seri.Sig as S
import qualified Seri.Lit as S
import qualified Seri.Exp as S
import qualified Seri.Dec as S
import qualified Seri.Loader as S

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

instance Lift S.Lit where
    lift (S.IntegerL i) = [| S.IntegerL i |]
    lift (S.CharL c) = [| S.CharL c |]

instance Lift S.Sig where
    lift (S.Sig n t) = [| S.Sig n t |]

instance Lift S.Exp where
    lift (S.LitE l) = [| S.LitE l |]
    lift (S.ConE s) = [| S.ConE s |]
    lift (S.VarE s) = [| S.VarE s |]
    lift (S.AppE f x) = [| S.AppE f x |]
    lift (S.LamE s b) = [| S.LamE s b |]
    lift (S.CaseE x k y n) = [| S.CaseE x k y n |]

instance Lift S.TyVar where
    lift (S.NormalTV n) = [| S.NormalTV n |]
    lift (S.NumericTV n) = [| S.NumericTV n |]
    
instance Lift S.Con where
    lift (S.Con n ts) = [| S.Con n ts |]

instance Lift S.Class where
    lift (S.Class n ts) = [| S.Class n ts |]

instance Lift S.Method where
    lift (S.Method n e) = [| S.Method n e |]

instance Lift S.Name where
    lift s = let str = S.unname s in [| S.name str |]

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
