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
        
      isprim :: Sig -> Bool
      isprim s = case (attemptM $ lookupVarInfo env s) of
                    Just _ -> True
                    Nothing -> False

      isfunt :: Type -> Bool
      isfunt t = 
        case unarrowsT t of
            _:_:_ -> True
            _ -> False
            

      -- return True if the given beta reduction should be applied with the
      -- given argument.
      shouldreduce :: Exp -> Bool
      shouldreduce (LitE {}) = True
      shouldreduce (ConE {}) = True
      shouldreduce (VarE {}) = True
      shouldreduce x | isfunt (typeof x) = True
      shouldreduce x | null (filter (not . isprim) (free x)) = True
      shouldreduce _ = False

  in case e of
       LitE {} -> e
       CaseE x ms ->
         let rx = elabme x
         in case (matches rx ms) of
            -- TODO: return error if no alternative matches?
            NoMatched -> CaseE rx [last ms]
            Matched vs b -> elabme $ letE vs b
            UnMatched ms' | mode == WHNF -> CaseE rx ms'
            UnMatched ms' | mode == SNF ->
                CaseE rx [Match p (elabmenms (bindingsP' p) b) | Match p b <- ms']
       AppE a b ->
           case (elabmeWHNF a, elabme b) of
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

             -- Only do reduction if there are no free variables in the
             -- argument. Otherwise things can blow up in unhappy ways.
             (LamE (Sig name _) body, rb) | shouldreduce rb ->
               let renamed = {-# SCC "BR" #-} alpharename (deleteall name freenms) body
               in elabme (reduce name rb renamed)

             -- Push application inside lambdas where possible.
             -- Rewrite:    ((\a -> b) x) y
             --             ((\a -> b y) x)
             -- With proper renaming.
             (AppE e@(LamE {}) x, y) ->
                let LamE s b = {-# SCC "PUSHL" #-} alpharename freenms e
                in AppE (elabme $ LamE s (AppE b y)) (elabme x)

             -- Push application inside case where possible.
             -- Rewrite:   (case x of { p1 -> m1; p2 -> m2 ; ... }) y
             --            (case x of { p1 -> m1 y; p2 -> m2 y; ... })
             -- With proper renaming
             (e@(CaseE {}), y) ->
                let CaseE x ms = {-# SCC "PUSHE" #-} alpharename freenms e
                in elabme $ CaseE x [Match p (AppE b y) | Match p b <- ms]
            
             (ra, rb) -> AppE (elabme ra) rb
       LamE {} | mode == WHNF -> e
       LamE s@(Sig n _) b | mode == SNF -> LamE s (elabmenm n b)
       ConE {} -> e
       VarE (Sig "Seri.Lib.Prelude.numeric" (NumT nt)) -> ConE (Sig ("#" ++ show (nteval nt)) (NumT nt))
       VarE (Sig n _) | n `elem` freenms -> e
       VarE s@(Sig _ ct) ->
           case (attemptM $ lookupVar env s) of
             Nothing -> e
             Just (pt, ve) -> elabme $ assign (assignments pt ct) ve 
        
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

-- reduce n v exp
-- Perform beta reduction in exp, replacing occurrences of variable n with v.
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

-- | Return a list of all local variable names introduced in the given
-- expression.
names :: Exp -> [Name]
names (LitE {}) = []
names (CaseE e ms) = 
  let namesm :: Match -> [Name]
      namesm (Match p b) = bindingsP' p ++ names b
  in nub $ concat (names e : map namesm ms)
names (AppE a b) = names a ++ names b
names (LamE (Sig n _) b) = nub $ n : names b
names (ConE {}) = []
names (VarE (Sig n _)) = []

-- | Rename any variable bindings in the given expression to names which do
-- not belong to the given list.
alpharename :: [Name] -> Exp -> Exp
alpharename [] e = e
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

deleteall :: Eq a => a -> [a] -> [a]
deleteall k [] = []
deleteall k (x:xs) | k == x = deleteall k xs
deleteall k (x:xs) = x : deleteall k xs

