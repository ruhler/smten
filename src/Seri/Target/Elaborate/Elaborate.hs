-------------------------------------------------------------------------------
-- Copyright (c) 2012      SRI International, Inc. 
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory under DARPA/AFRL contract (FA8750-10-C-0237)
-- ("CTSRD"), as part of the DARPA CRASH research programme.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
-------------------------------------------------------------------------------
--
-- Authors: 
--   Richard Uhler <ruhler@csail.mit.edu>
-- 
-------------------------------------------------------------------------------

-- | Target for reducing a seri expression to a normal form.
module Seri.Target.Elaborate.Elaborate (
    simplify, elaborate,
    ) where

import Data.Generics
import Data.Maybe(catMaybes, fromMaybe)
import Data.List(nub, (\\))

import Seri.Failable
import Seri.Lambda

-- | Simplify an expression as much as possible.
simplify :: Exp -> Exp
simplify = elaborate []

-- | Reduce the given expression as much as possible.
elaborate :: Env   -- ^ context under which to evaluate the expression
          -> Exp   -- ^ expression to evaluate
          -> Exp   -- ^ elaborated expression
elaborate env e =
  case e of
    (LitE {}) -> e
    (CaseE x (m:ms)) ->
        let rx = elaborate env x
            Match p b = m
           -- Don't make a case statement empty, because we want to 
           -- keep enough information to determine the type of the
           -- case statement.
           -- TODO: maybe we should return "error" instead?
        in case (match p rx, null ms) of
              (Succeeded vs, _) -> elaborate env $ reduces vs b
              (Failed, False) -> elaborate env (CaseE rx ms)
              _ -> CaseE rx [Match p (simplify b) | Match p b <- (m:ms)]
    (AppE a b) ->
        case (elaborate env a, elaborate env b) of
          (AppE (VarE (Sig "Seri.Lib.Prelude.__prim_add_Integer" _)) (LitE (IntegerL ia)), (LitE (IntegerL ib))) -> integerE (ia + ib)
          (AppE (VarE (Sig "Seri.Lib.Prelude.__prim_sub_Integer" _)) (LitE (IntegerL ia)), (LitE (IntegerL ib))) -> integerE (ia - ib)
          (AppE (VarE (Sig "Seri.Lib.Prelude.__prim_mul_Integer" _)) (LitE (IntegerL ia)), (LitE (IntegerL ib))) -> integerE (ia * ib)
          (AppE (VarE (Sig "Seri.Lib.Prelude.<" _)) (LitE (IntegerL ia)), (LitE (IntegerL ib))) -> boolE (ia < ib)
          (AppE (VarE (Sig "Seri.Lib.Prelude.>" _)) (LitE (IntegerL ia)), (LitE (IntegerL ib))) -> boolE (ia > ib)
          (AppE (VarE (Sig "Seri.Lib.Prelude.__prim_eq_Integer" _)) (LitE (IntegerL ia)), (LitE (IntegerL ib))) -> boolE (ia == ib)
          (LamE (Sig name _) body, rb) ->
             let freenames = map (\(Sig n _) -> n) (free rb)
                 body' = alpharename (freenames \\ [name]) body
             in elaborate env (reduce name rb body')
          (ra, rb) -> AppE ra rb
    (LamE s b) -> LamE s (simplify b)
    (ConE s) -> e
    (VarE s@(Sig _ ct)) ->
        case (attemptM $ lookupVar env s) of
          Nothing -> e
          Just (pt, ve) -> elaborate env $ assign (assignments pt ct) ve
        
data MatchResult = Failed | Succeeded [(Name, Exp)] | Unknown

-- Match an expression against a pattern.
match :: Pat -> Exp -> MatchResult
match (ConP _ nm []) (ConE (Sig n _)) | n == nm = Succeeded []
match (ConP t n ps) (AppE ae be) | not (null ps)
  = case (match (ConP t n (init ps)) ae, match (last ps) be) of
        (Succeeded as, Succeeded bs) -> Succeeded (as ++ bs)
        (Failed, _) -> Failed
        (Succeeded _, Failed) -> Failed
        _ -> Unknown
match (IntegerP i) (LitE (IntegerL i')) | i == i' = Succeeded []
match (VarP (Sig nm _)) e = Succeeded [(nm, e)]
match (WildP _) _ = Succeeded []
match _ x | iswhnf x = Failed
match _ _ = Unknown

-- iswhnf exp
--  Return True if the expression is in weak head normal form.
--  TODO: how should we handle primitives?
iswhnf :: Exp -> Bool
iswhnf (LitE _) = True
iswhnf (LamE _ _) = True
iswhnf x
 = let iscon :: Exp -> Bool
       iscon (ConE _) = True
       iscon (AppE f _) = iscon f
       iscon _ = False
   in iscon x

-- reduce n v exp
-- Perform beta reduction in exp, replacing occurrences of variable n with v.
reduce :: Name -> Exp -> Exp -> Exp
reduce n v e = reduces [(n, v)] e

-- reduces vs exp
-- Perform multiple simultaneous beta reduction in exp, replacing occurrences
-- of variable n with v if (n, v) is in vs.
reduces :: [(Name, Exp)] -> Exp -> Exp
reduces _ e@(LitE _) = e
reduces vs (CaseE e ms) =
    let reducematch :: Match -> Match
        reducematch (Match p b) =
         let bound = bindingsP' p
             vs' = filter (\(n, _) -> not (n `elem` bound)) vs
         in Match p (reduces vs' b)
    in CaseE (reduces vs e) (map reducematch ms)
reduces vs (AppE a b) = AppE (reduces vs a) (reduces vs b)
reduces vs e@(LamE (Sig ln t) b)
  = LamE (Sig ln t) (reduces (filter (\(n, _) -> n /= ln) vs) b)
reduces _ e@(ConE _) = e
reduces vs e@(VarE (Sig vn _)) =
    case lookup vn vs of
        (Just v) -> v
        Nothing -> e

-- | Return a list of all variables in the given expression.
names :: Exp -> [Name]
names (LitE {}) = []
names (CaseE e ms) = 
  let namesm :: Match -> [Name]
      namesm (Match p b) = bindingsP' p ++ names b
  in nub $ concat (names e : map namesm ms)
names (AppE a b) = names a ++ names b
names (LamE (Sig n _) b) = nub $ n : names b
names (ConE {}) = []
names (VarE (Sig n _)) = [n]

-- | Rename any variable bindings in the given expression to names which do
-- not belong to the given list.
alpharename :: [Name] -> Exp -> Exp
alpharename bad e =
  let isgood :: String -> Bool
      isgood s = not (s `elem` bad)

      isgoodnew :: String -> Bool
      isgoodnew s = isgood s && not (s `elem` names e)

      -- get the new name for the given name.
      newname :: String -> String
      newname n | isgood n = n
      newname n = head (filter isgoodnew [n ++ show i | i <- [0..]])
    
      repat :: Pat -> Pat
      repat (ConP t n ps) = ConP t n (map repat ps)
      repat (VarP (Sig n t)) = VarP (Sig (newname n) t)
      repat p@(IntegerP {}) = p
      repat p@(WildP {}) = p

      rematch :: [Name] -> Match -> Match
      rematch bound (Match p b) = 
        let p' = repat p
            b' = rename (bindingsP' p ++ bound) b
        in Match p' b'

      -- Do alpha renaming in an expression given the list of bound variable
      -- names before renaming.
      rename :: [Name] -> Exp -> Exp
      rename _ e@(LitE {}) = e
      rename bound (CaseE e ms)
        = CaseE (rename bound e) (map (rematch bound) ms)
      rename bound (AppE a b) = AppE (rename bound a) (rename bound b)
      rename bound (LamE (Sig n t) b)
        = LamE (Sig (newname n) t) (rename (n : bound) b)
      rename _ e@(ConE {}) = e
      rename bound (VarE (Sig n t)) | n `elem` bound = VarE (Sig (newname n) t) 
      rename _ e@(VarE {}) = e
  in rename [] e

