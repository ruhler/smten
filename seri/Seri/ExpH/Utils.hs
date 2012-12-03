
{-# LANGUAGE PatternGuards #-}

module Seri.ExpH.Utils (
    transform,
    impliedByBoolH,
    runio, caseEH, ifEH,
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

-- Assuming the given expression has value True or False, what can we easily
-- infer about the free variables in that expression?
impliedByBoolH :: Bool -> ExpH -> [(ExpH, ExpH)]
impliedByBoolH b e
 | CaseEH x k y n <- e
 , Just kv <- de_boolEH (conEH k)
 , y == (boolEH (not b)) = [(e, boolEH b)] ++ impliedByBoolH (not kv) x ++ impliedByBoolH b n
 | CaseEH x k y n <- e
 , Just kv <- de_boolEH (conEH k)
 , n == (boolEH (not b)) = [(e, boolEH b)] ++ impliedByBoolH kv x ++ impliedByBoolH b y
 | otherwise = [(e, boolEH b)]

-- | Given a Seri expression of type IO a,
-- returns the Seri expression of type a which results from running the IO
-- computation.
runio :: ExpH -> IO ExpH
runio (ErrorEH _ msg) = error $ "seri: " ++ msg
runio e = fromMaybe (error $ "runio got non-IO: " ++ pretty e) (de_ioEH e)

caseEH :: ExpH -> Sig -> ExpH -> ExpH -> ExpH
caseEH x k@(Sig nk _) y n
 | (ConEH (Sig s _), vs) <- de_appsEH x
    = if s == nk then appsEH y vs else n
 | Just (_, msg) <- de_errorEH x = errorEH (typeof n) msg
 | Just b <- de_boolEH (conEH k) =
    let ify = impliedByBoolH b x
        ifn = impliedByBoolH (not b) x
        y' = transform (flip lookup ify) y
        n' = transform (flip lookup ifn) n
    in CaseEH x k y' n'
 | otherwise = CaseEH x k y n

ifEH :: ExpH -> ExpH -> ExpH -> ExpH
ifEH p a b = caseEH p (Sig (name "True") boolT) a b

