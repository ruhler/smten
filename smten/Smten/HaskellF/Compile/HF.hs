
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | The HaskellF compilation monad.
module Smten.HaskellF.Compile.HF (
    HFS(..), HF, runHF,
    asks, local, throw,
    ) where

import Control.Monad.Reader

import Smten.Name
import Smten.Location
import Smten.Type
import Smten.Dec
import Smten.Failable

data HFS = HFS {
    hfs_env :: Env,

    -- | A list of type variables already in scope.
    hfs_tyvars :: [Name],
    
    -- | A map from Type to variable type name.
    -- This is used to replace numeric type operations with variable types, to
    -- get around issues with ghc doing math.
    hfs_retype :: [(Type, Name)]

}

type HF = ReaderT HFS Failable

instance MonadErrorSL HF where
    errloc = return $ Location "HaskellF Unknown" 0 0

runHF :: Env -> HF a -> Failable a
runHF e x = runReaderT x (HFS e [] [])

