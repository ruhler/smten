
{-# LANGUAGE PatternGuards #-}

module Seri.ExpH.Utils (transform) where

import Seri.Type
import Seri.ExpH.ExpH
import Seri.Sig

instance Assign ExpH where
   assignl f e =
    let me = assignl f 
        mt = assignl f
    in case e of
         LitEH {} -> e
         ConEH (Sig n t) -> ConEH (Sig n (mt t))
         VarEH (Sig n t) -> VarEH (Sig n (mt t))
         AppEH m a b -> AppEH m (me a) (me b)
         LamEH m (Sig n t) b -> LamEH m (Sig n (mt t)) $ \x -> (me (b x))
         CaseEH m x (Sig kn kt) y n -> CaseEH m (me x) (Sig kn (mt kt)) (me y) (me n)

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
       ConEH {} -> e
       VarEH {} -> e 
       AppEH _ f x -> AppEH ES_None (me f) (me x)
       LamEH _ s f -> LamEH ES_None s $ \x -> me (f x)
       CaseEH _ x k y d -> CaseEH ES_None (me x) k (me y) (me d)

