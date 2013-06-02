
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.CodeGen.CG (CG, cg_env, runCG, asks) where

import Control.Monad.Reader

import Smten.Failable
import Smten.Dec

data CGR = CGR {
    cg_env :: Env
}

type CG = ReaderT CGR Failable

instance MonadErrorSL CG where
    errloc = lift errloc

runCG :: Env -> CG a -> Failable a
runCG e q = runReaderT q (CGR e)

