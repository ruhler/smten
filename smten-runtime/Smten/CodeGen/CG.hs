
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.CodeGen.CG (
    CG, cg_env, cg_tyvars, cg_retype,
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
    cg_tyvars :: [Name],
    
    -- | A map from Type to variable type name.
    -- This is used to replace numeric type operations with variable types, to 
    -- get around issues with ghc doing math.
    cg_retype :: [(Type, Name)]
}

type CG = ReaderT CGR Failable

instance MonadErrorSL CG where
    errloc = lift errloc

withTyVars :: (VarTs a) => a -> CG b -> CG b
withTyVars x = local (\s -> s { cg_tyvars = map fst (varTs x) ++ cg_tyvars s })

runCG :: Env -> CG a -> Failable a
runCG e q = runReaderT q (CGR e [] [])

