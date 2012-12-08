
{-# LANGUAGE PatternGuards #-}

module Seri.ExpH.Utils (
    transform,
    runio,
    ) where

import Data.Maybe

import Seri.Type
import Seri.Name
import Seri.Sig
import Seri.Ppr
import Seri.ExpH.Ppr
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar

-- Perform a generic transformation on an expression.
-- Applies the given function to each subexpression. Any matching
-- subexpression is replaced with the returned value, otherwise it continues
-- to recurse.
transform :: (ExpH -> Maybe ExpH) -> ExpH -> ExpH
transform g e | Just v <- g e = v
transform g e =
  let me = transform g
  in case e of
       LitEH {} -> e
       ConEH n s xs -> ConEH n s (map me xs)
       VarEH {} -> e 
       PrimEH _ _ f xs -> f (map me xs)
       AppEH f x i -> AppEH (me f) (me x) (me i)
       LamEH s t f -> lamEH s t $ \x -> me (f x)
       CaseEH x k y d -> caseEH (me x) k (me y) (me d)
       ErrorEH {} -> e

-- | Given a Seri expression of type IO a,
-- returns the Seri expression of type a which results from running the IO
-- computation.
runio :: ExpH -> IO ExpH
runio e
 | Just (_, msg) <- de_errorEH e = error $ "seri: " ++ msg
 | Just io <- de_ioEH e = io
 | otherwise = error $ "runio got non-IO: " ++ pretty (un_letEH e)

