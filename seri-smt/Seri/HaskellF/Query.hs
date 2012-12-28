
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}

-- | Helper functions for working with queries and Symbolic stuff.
module Seri.HaskellF.Query (
    qS, assertS, realizeS, realizeS',
    ) where

import Data.Functor ((<$>))
import Data.Maybe

import Seri.Ppr
import Seri.Type
import Seri.ExpH
import Seri.SMT.Query
import Seri.SMT.Primitives
import qualified Seri.HaskellF.Lib.Prelude as S
import qualified Seri.HaskellF.Lib.SMT as S
import Seri.HaskellF.Symbolic

instance (Symbolic a) => SeriS (Query ExpH) (S.Query a) where
    seriS = box . seriEH
    de_seriS = de_seriEH . unbox
    

qS :: (Symbolic a) => S.Query a -> Query a
qS s
 | Just q <- de_seriS s = box <$> q
 | otherwise = error $ "qS: " ++ pretty (unbox s)


assertS :: S.Bool -> Query ()
assertS p = qS (S.assert p) >> return ()

realizeS' :: (Symbolic a) => a -> Realize a
realizeS' = fmap box . realize . unbox

de_seriEH' :: (SeriEH a) => ExpH -> a
de_seriEH' x = fromMaybe (error $ "de_seriEH': " ++ pretty x) (de_seriEH x)

realizeS :: (Symbolic a, SeriEH b) => a -> Realize b
realizeS = fmap de_seriEH' . realize . unbox

