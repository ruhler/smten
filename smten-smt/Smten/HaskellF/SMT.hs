
{-# LANGUAGE PatternGuards #-}

module Smten.HaskellF.SMT (
    symbolicHF, assertHF, queryHF, queryHFC,
    ) where

import Data.Functor((<$>))
import Data.Maybe (fromMaybe)

import Smten.SMT.SMT
import Smten.HaskellF.HaskellF
import qualified Smten.HaskellF.Lib.Prelude as S
import qualified Smten.HaskellF.Lib.Symbolic as S


symbolicHF :: (HaskellF a) => S.Symbolic a -> Symbolic a
symbolicHF s = box <$> de_symbolicEH (unbox s)

assertHF :: S.Bool -> Symbolic ()
assertHF x = symbolicHF (S.assert x) >> return ()

queryHF :: (HaskellF a) => Symbolic a -> SMT (Maybe a)
queryHF s = do
    r <- query (realize . unbox <$> s)
    return (box <$> r)

queryHFC :: (SmtenHF c f) => Symbolic f -> SMT (Maybe c)
queryHFC s = do
    r <- queryHF s
    return (fromMaybe (error "queryHFC") . de_smtenHF <$> r)
