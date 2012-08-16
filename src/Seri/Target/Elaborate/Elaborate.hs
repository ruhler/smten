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

-- | Target for elaborating seri expressions.
module Seri.Target.Elaborate.Elaborate (
    Mode(..), elaborate,
    ) where

import Debug.Trace

import Data.Bits
import Data.Maybe(catMaybes, fromMaybe)
import Data.List(nub, (\\))

import Seri.Bit
import Seri.Failable
import Seri.Lambda

data Mode = WHNF -- ^ elaborate to weak head normal form.
          | SNF  -- ^ elaborate to sharing normal form.
    deriving (Show, Eq)

-- | Elaborate an expression under the given mode.
elaborate :: Mode  -- ^ Elaboration mode
          -> Env   -- ^ context under which to evaluate the expression
          -> Exp   -- ^ expression to evaluate
          -> Exp   -- ^ elaborated expression
elaborate mode env = elaborate' mode env []

elaborate' :: Mode  -- ^ Elaboration mode
          -> Env   -- ^ context under which to evaluate the expression
          -> [Name] -- ^ free variables in scope
          -> Exp   -- ^ expression to evaluate
          -> Exp   -- ^ elaborated expression
elaborate' mode env freenms e =
  let elabme = elaborate' mode env freenms
      elabmeWHNF = elaborate' WHNF env freenms
      elabmenms nms = elaborate' mode env (nms ++ freenms)
      elabmenm n = elaborate' mode env (n:freenms)
        
      hasNonPrimFree :: [Name] -> Exp -> Bool
      hasNonPrimFree nms e =
         case e of
           LitE {} -> False
           CaseE x ms ->
             let hm :: Match -> Bool
                 hm (Match p b) = hasNonPrimFree (bindingsP' p ++ nms) b
             in hasNonPrimFree nms x || any hm ms
           AppE a b -> hasNonPrimFree nms a || hasNonPrimFree nms b
           LamE (Sig n _) b -> hasNonPrimFree (n:nms) b
           ConE {} -> False
           VarE (Sig n _) | n `elem` nms -> False
           VarE s -> 
             case (attemptM $ lookupVarInfo env s) of
                Just _ -> False
                Nothing -> True
  in case e of
       LitE {} -> {-# SCC "LIT" #-} e
       CaseE x ms -> {-# SCC "CASE" #-}
         let rx = elabme x
         in case (matches rx ms) of
            -- TODO: return error if no alternative matches?
            NoMatched -> CaseE rx [last ms]
            Matched vs b -> elabme $ letE vs b
            UnMatched ms' | mode == WHNF -> CaseE rx ms'
            UnMatched ms' | mode == SNF ->
                CaseE rx [Match p (elabmenms (bindingsP' p) b) | Match p b <- ms']
       AppE a b -> {-# SCC "APP" #-}
           case (elabmeWHNF a, elabme b) of
             -- Only do reduction if there are no free variables in the
             -- argument. Otherwise things can blow up in unhappy ways.
             -- This does not require alpha renaming.
             (LamE (Sig name _) body, rb) | not (hasNonPrimFree [] rb) ->
                elabme (reduce name rb body)
    
             -- But do inline simple variables.
             -- This requires alpha renaming.
             (LamE (Sig name _) body, rb@(VarE {})) ->
                 elabme (reducern [(name, rb)] body)

             (VarE (Sig "Seri.Lib.Prelude.valueof" t), _) ->
                let NumT nt = head $ unarrowsT t
                in integerE (nteval nt)
             (VarE (Sig "Seri.Lib.Bit.__prim_zeroExtend_Bit" (AppT _ (AppT _ (NumT wt)))), (AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (AppT _ (AppT _ (NumT ws))))) (LitE (IntegerL ia)))) -> AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (arrowsT [integerT, bitT (nteval wt)]))) (integerE $ bv_value (bv_zero_extend (nteval wt - nteval ws) (bv_make (nteval ws) ia)))
             (VarE (Sig "Seri.Lib.Bit.__prim_truncate_Bit" (AppT _ (AppT _ (NumT wt)))), (AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (AppT _ (AppT _ (NumT ws))))) (LitE (IntegerL ia)))) -> AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (arrowsT [integerT, bitT (nteval wt)]))) (integerE $ bv_value (bv_truncate (nteval wt) (bv_make (nteval ws) ia)))
             (AppE (VarE (Sig "Seri.Lib.Prelude.__prim_eq_Char" _)) (LitE (CharL ia)), LitE (CharL ib)) -> boolE (ia == ib)
             (AppE (VarE (Sig "Seri.Lib.Prelude.__prim_eq_Integer" _))  (LitE (IntegerL ia)), LitE (IntegerL ib)) -> boolE (ia == ib)
             (AppE (VarE (Sig "Seri.Lib.Prelude.__prim_add_Integer" _)) (LitE (IntegerL ia)), LitE (IntegerL ib)) -> integerE (ia + ib)
             (AppE (VarE (Sig "Seri.Lib.Prelude.__prim_sub_Integer" _)) (LitE (IntegerL ia)), LitE (IntegerL ib)) -> integerE (ia - ib)
             (AppE (VarE (Sig "Seri.Lib.Prelude.__prim_mul_Integer" _)) (LitE (IntegerL ia)), LitE (IntegerL ib)) -> integerE (ia * ib)
             (AppE (VarE (Sig "Seri.Lib.Prelude.<" _))                  (LitE (IntegerL ia)), LitE (IntegerL ib)) -> boolE (ia < ib)
             (AppE (VarE (Sig "Seri.Lib.Prelude.>" _))                  (LitE (IntegerL ia)), LitE (IntegerL ib)) -> boolE (ia > ib)
             -- TODO: there has got to be a better way to specify this than
             -- writing it all out. Pattern abstractions anyone?
             (AppE (VarE (Sig "Seri.Lib.Bit.__prim_eq_Bit" _)) (AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (AppT _ (AppT _ (NumT w))))) (LitE (IntegerL ia))), AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" _)) (LitE (IntegerL ib))) -> boolE $ bv_make (nteval w) ia == bv_make (nteval w) ib
             (AppE (VarE (Sig "Seri.Lib.Bit.__prim_add_Bit" _)) (AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (AppT _ (AppT _ (NumT w))))) (LitE (IntegerL ia))), AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" _)) (LitE (IntegerL ib))) -> AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (arrowsT [integerT, bitT (nteval w)]))) (integerE $ bv_value (bv_make (nteval w) ia + bv_make (nteval w) ib))
             (AppE (VarE (Sig "Seri.Lib.Bit.__prim_sub_Bit" _)) (AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (AppT _ (AppT _ (NumT w))))) (LitE (IntegerL ia))), AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" _)) (LitE (IntegerL ib))) -> AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (arrowsT [integerT, bitT (nteval w)]))) (integerE $ bv_value (bv_make (nteval w) ia - bv_make (nteval w) ib))
             (AppE (VarE (Sig "Seri.Lib.Bit.__prim_mul_Bit" _)) (AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (AppT _ (AppT _ (NumT w))))) (LitE (IntegerL ia))), AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" _)) (LitE (IntegerL ib))) -> AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (arrowsT [integerT, bitT (nteval w)]))) (integerE $ bv_value (bv_make (nteval w) ia * bv_make (nteval w) ib))
             (AppE (VarE (Sig "Seri.Lib.Bit.__prim_or_Bit" _)) (AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (AppT _ (AppT _ (NumT w))))) (LitE (IntegerL ia))), AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" _)) (LitE (IntegerL ib))) -> AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (arrowsT [integerT, bitT (nteval w)]))) (integerE $ bv_value (bv_make (nteval w) ia .|. bv_make (nteval w) ib))
             (AppE (VarE (Sig "Seri.Lib.Bit.__prim_and_Bit" _)) (AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (AppT _ (AppT _ (NumT w))))) (LitE (IntegerL ia))), AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" _)) (LitE (IntegerL ib))) -> AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (arrowsT [integerT, bitT (nteval w)]))) (integerE $ bv_value (bv_make (nteval w) ia .&. bv_make (nteval w) ib))
             (AppE (VarE (Sig "Seri.Lib.Bit.__prim_lsh_Bit" _)) (AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (AppT _ (AppT _ (NumT w))))) (LitE (IntegerL ia))), LitE (IntegerL ib)) -> AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (arrowsT [integerT, bitT (nteval w)]))) (integerE $ bv_value (bv_make (nteval w) ia `shiftL` fromInteger ib))
             (AppE (VarE (Sig "Seri.Lib.Bit.__prim_rshl_Bit" _)) (AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (AppT _ (AppT _ (NumT w))))) (LitE (IntegerL ia))), LitE (IntegerL ib)) -> AppE (VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (arrowsT [integerT, bitT (nteval w)]))) (integerE $ bv_value (bv_make (nteval w) ia `shiftR` fromInteger ib))

             (ra, rb) -> AppE (elabme ra) rb
       LamE {} | mode == WHNF -> {-# SCC "LAM_WHNF" #-} e
       LamE s@(Sig n _) b | mode == SNF -> {-# SCC "LAM_SNF" #-} LamE s (elabmenm n b)
       ConE {} -> {-# SCC "CON" #-} e
       VarE (Sig "Seri.Lib.Prelude.numeric" (NumT nt)) -> {-# SCC "NUMERIC" #-} ConE (Sig ("#" ++ show (nteval nt)) (NumT nt))
       VarE (Sig n _) | n `elem` freenms -> {-# SCC "VAR_FREE" #-} e
       VarE s@(Sig _ ct) -> {-# SCC "VAR_LOOKUP" #-}
           case (attemptM $ lookupVar env s) of
             Nothing -> e
             Just (pt, ve) -> elabme $ assignexp (assignments pt ct) ve 

assignexp :: [(Name, Type)] -> Exp -> Exp
assignexp = assign
        
data MatchResult = Failed | Succeeded [(Sig, Exp)] | Unknown
data MatchesResult
 = Matched [(Sig, Exp)] Exp
 | UnMatched [Match]
 | NoMatched

-- Match an expression against a sequence of alternatives.
matches :: Exp -> [Match] -> MatchesResult
matches _ [] = NoMatched
matches x ms@((Match p b):_) =
  case match p x of 
    Failed -> matches x (tail ms)
    Succeeded vs -> Matched vs b
    Unknown -> UnMatched ms

-- Match an expression against a pattern.
match :: Pat -> Exp -> MatchResult
match (ConP _ nm ps) e =
  case unappsE e of
    ((ConE (Sig n _)):_) | n /= nm -> Failed
    ((ConE {}):args) ->
       let mrs = [match p e | (p, e) <- zip ps args]
           join (Succeeded as) (Succeeded bs) = Succeeded (as ++ bs)
           join Failed _ = Failed
           join (Succeeded _) Failed = Failed
           join _ _ = Unknown
       in foldl join (Succeeded []) mrs
    _ -> Unknown
match (IntegerP i) (LitE (IntegerL i')) | i == i' = Succeeded []
match (IntegerP i) (LitE {}) = Failed
match (VarP s) e = Succeeded [(s, e)]
match (WildP _) _ = Succeeded []
match _ _ = Unknown


-- Return True if the expression has the given name as a free variable.
hasfree :: Name -> Exp -> Bool
hasfree n =
  let hn :: Exp -> Bool
      hn (LitE {}) = False
      hn (CaseE e ms) =
        let hnp :: Pat -> Bool
            hnp (ConP _ _ ps) = any hnp ps
            hnp (VarP (Sig nm _)) = n == nm
            hnp (IntegerP {}) = False
            hnp (WildP {}) = False
            
            hnm :: Match -> Bool
            hnm (Match p b) = if hnp p then False else hn b
        in hn e || any hnm ms
      hn (AppE a b) = hn a || hn b
      hn (LamE (Sig nm _) b) | nm == n = False
      hn (LamE s b) = hn b
      hn (ConE {}) = False
      hn (VarE (Sig nm _)) = nm == n
  in hn

-- reducern n v exp
-- Perform beta reduction in exp, replacing occurrences of variable n with v.
-- This does alpha-renaming where needed.
reducern :: [(Name, Exp)] -> Exp -> Exp
reducern [] e = e
reducern m e =
     -- Given: a list of names being bound
     --        the things to be reduced
     -- Return: - a list of updated names needed for those things being bound
     --         because they would otherwise capture free variables,
     --         - an updated list of things to be bound based on that
     --         renaming.
     --         
 let renames :: [Sig] -> ([(Sig, Sig)], [(Name, Exp)])
     renames ns = 
        let nsnames = [n | Sig n _ <- ns]
            m' = filter (\(n, _) -> n `notElem` nsnames) m
            badname n = any (\(_, v) -> hasfree n v) m'
            newname n = head (filter (\n' -> not (badname n') && n' `notElem` nsnames) [n ++  show i | i <- [0..]])
            bads = filter (\(Sig n _) -> badname n) ns
            rename = [(s, Sig (newname n) t) | s@(Sig n t) <- bads]
            updates = [(n, VarE s) | (Sig n _, s) <- rename]
        in (rename, updates ++ m')
 in case e of
       LitE {} -> e
       CaseE e ms ->
         let rm :: Match -> Match
             rm (Match p b) =
               let (rename, m') = renames (bindingsP p)
                   repat :: Pat -> Pat
                   repat (ConP t n ps) = ConP t n (map repat ps)
                   repat (VarP s) = VarP (fromMaybe s (lookup s rename))
                   repat p@(IntegerP {}) = p
                   repat p@(WildP {}) = p
               in Match (if null rename then p else repat p) (reducern m' b)
         in CaseE (reducern m e) (map rm ms)
       AppE a b -> AppE (reducern m a) (reducern m b)
       LamE s b ->
         let (rename, m') = renames [s]
         in LamE (fromMaybe s (lookup s rename)) (reducern m' b)
       ConE {} -> e
       VarE (Sig nm t) ->
         case lookup nm m of
            Just v -> v
            Nothing -> e

-- reduce n v exp
-- Perform beta reduction in exp, replacing occurrences of variable n with v.
-- This does not do any renaming.
reduce :: Name -> Exp -> Exp -> Exp
reduce n v =
  let reduceme e =
        case e of
           LitE {} -> e
           CaseE e ms ->
             let reducematch :: Match -> Match
                 reducematch m@(Match p _) | n `elem` (bindingsP' p) = m
                 reducematch (Match p b) = Match p (reduceme b)
             in CaseE (reduceme e) (map reducematch ms)
           AppE a b -> AppE (reduceme a) (reduceme b)
           LamE (Sig nm t) b | n == nm -> e
           LamE s b -> LamE s (reduceme b)
           ConE {} -> e
           VarE (Sig nm _) | n == nm -> v
           VarE {} -> e
  in reduceme

