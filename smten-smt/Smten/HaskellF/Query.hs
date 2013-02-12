
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}

-- | Helper functions for working with queries and Symbolic stuff.
module Smten.HaskellF.Query (
    qS, assertS, realizeS, realizeS',
    ) where

import Data.Functor ((<$>))
import Data.Maybe

import Smten.Ppr
import Smten.Type
import Smten.ExpH
import Smten.SMT.Query
import Smten.SMT.Primitives
import qualified Smten.HaskellF.Lib.Prelude as S
import qualified Smten.HaskellF.Lib.SMT as S
import Smten.HaskellF.Symbolic

instance (Symbolic a) => SmtenS (Query ExpH) (S.Query a) where
    smtenS = box . smtenEH
    de_smtenS = de_smtenEH . unbox
    

qS :: (Symbolic a) => S.Query a -> Query a
qS s
 | Just q <- de_smtenS s = box <$> q
 | otherwise = error $ "qS: " ++ pretty (unbox s)


assertS :: S.Bool -> Query ()
assertS p = qS (S.assert p) >> return ()

realizeS' :: (Symbolic a) => a -> Realize a
realizeS' = fmap box . realize . unbox

de_smtenEH' :: (SmtenEH a) => ExpH -> a
de_smtenEH' x = fromMaybe (error $ "de_smtenEH': " ++ pretty x) (de_smtenEH x)

realizeS :: (Symbolic a, SmtenEH b) => a -> Realize b
realizeS = fmap de_smtenEH' . realize . unbox

