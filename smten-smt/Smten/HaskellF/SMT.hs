
{-# LANGUAGE PatternGuards #-}

module Smten.HaskellF.SMT (
    symbolicHF, assertHF, queryHF, queryHFC, runSymbolicHF,
    ) where

import Data.Functor((<$>))
import Data.Maybe (fromMaybe)

import Smten.ExpH
import Smten.SMT.SMT
import Smten.HaskellF.HaskellF
import Smten.SMT.Solver(Solver)
import qualified Smten.HaskellF.Lib.Prelude as S
import qualified Smten.HaskellF.Lib.Symbolic as S


symbolicHF :: (HaskellF a) => S.Symbolic a -> Symbolic a
symbolicHF s = box <$> de_symbolicEH (unbox s)

assertHF :: S.Bool -> Symbolic ()
assertHF x = symbolicHF (applyHF S.assert x) >> return ()

queryHF :: (HaskellF a) => Symbolic a -> SMT (Maybe a)
queryHF s = do
    r <- query (realize . unbox <$> s)
    return (box <$> r)

runSymbolicHF :: (HaskellF a) => Solver -> Symbolic a -> IO (Maybe a)
runSymbolicHF solver s = do
    r <- runSymbolic solver (realize . unbox <$> s)
    return (box <$> r)

queryHFC :: (SmtenEH c, HaskellF f) => Symbolic f -> SMT (Maybe c)
queryHFC s = do
    r <- queryHF s
    return (fromMaybe (error "queryHFC") . de_smtenHF <$> r)
