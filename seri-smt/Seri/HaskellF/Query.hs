
-- | Helper functions for working with queries and Symbolic stuff.
module Seri.HaskellF.Query (
    freeS, assertS, realizeS,
    ) where

import Data.Functor ((<$>))
import Data.Maybe

import Seri.Ppr
import Seri.Type
import Seri.ExpH
import Seri.SMT.Query
import qualified Seri.HaskellF.Lib.Prelude as S
import Seri.HaskellF.Symbolic

freeS :: (Symbolic a) => Query a
freeS = 
  let f :: Query a -> a
      f _ = undefined

      q = box <$> free (seriT (f q))
  in q

assertS :: S.Bool -> Query ()
assertS = assert . unbox

de_seriEH' :: (SeriEH a) => ExpH -> a
de_seriEH' x = fromMaybe (error $ "de_seriEH': " ++ pretty x) (de_seriEH x)

realizeS :: (Symbolic a, SeriEH b) => a -> Realize b
realizeS = fmap de_seriEH' . realize . unbox

