
{-# LANGUAGE PatternGuards #-}

module Seri.ExpH.Utils (
    transform,
    substituteH,
    impliedByTrueH, impliedByFalseH,
    runio,
    ) where

import Data.Maybe

import Seri.Type
import Seri.Name
import Seri.Sig
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar


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
       PrimEH s f xs -> f (map me xs)
       AppEH _ f x -> appEH (me f) (me x)
       LamEH _ s f -> lamEH s $ \x -> me (f x)
       CaseEH _ x k y d -> caseEH (me x) k (me y) (me d)

substituteH :: (Name -> Maybe ExpH) -> ExpH -> ExpH
substituteH f =
  let g (VarEH (Sig n _)) = f n
      g _ = Nothing
  in transform g

-- Todo: look for eq, not, and, and or calls to get more info here.
impliedByTrueH :: ExpH -> [(Name, ExpH)]
impliedByTrueH e = []

-- Todo: look for eq, not, and, and or calls to get more info here.
impliedByFalseH :: ExpH -> [(Name, ExpH)]
impliedByFalseH e = []

-- | Given a Seri expression of type IO a,
-- returns the Seri expression of type a which results from running the IO
-- computation.
runio :: ExpH -> IO ExpH
runio e = fromMaybe (error "runio got non-IO") (de_ioEH e)

