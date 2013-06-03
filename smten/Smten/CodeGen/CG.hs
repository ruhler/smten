
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.CodeGen.CG (
    CG, cg_env, cg_tyvars,
    withTyVars,
    runCG, asks, local
    ) where

import Control.Monad.Reader

import Smten.Failable
import Smten.Name
import Smten.Type
import Smten.Dec

data CGR = CGR {
    cg_env :: Env,
    cg_tyvars :: [Name]
}

type CG = ReaderT CGR Failable

instance MonadErrorSL CG where
    errloc = lift errloc

withTyVars :: (VarTs a) => a -> CG b -> CG b
withTyVars x = local (\s -> s { cg_tyvars = map fst (varTs x) ++ cg_tyvars s })

runCG :: Env -> CG a -> Failable a
runCG e q = runReaderT q (CGR e [])

