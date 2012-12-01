
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
import Seri.Ppr
import Seri.ExpH.Ppr
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
         AppEH a b -> AppEH (me a) (me b)
         LamEH (Sig n t) b -> LamEH (Sig n (mt t)) $ \x -> (me (b x))
         CaseEH x (Sig kn kt) y n -> CaseEH (me x) (Sig kn (mt kt)) (me y) (me n)
         ErrorEH t m -> ErrorEH (mt t) m

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
       AppEH f x -> appEH (me f) (me x)
       LamEH s f -> lamEH s $ \x -> me (f x)
       CaseEH x k y d -> caseEH (me x) k (me y) (me d)
       ErrorEH {} -> e

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
runio (ErrorEH _ msg) = error $ "seri: " ++ msg
runio e = fromMaybe (error $ "runio got non-IO: " ++ pretty e) (de_ioEH e)

