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

{-# LANGUAGE FlexibleInstances #-}

-- | Target for elaborating seri expressions.
module Seri.Target.Elaborate.Elaborate3 (
    Mode(..), elaborate,
    ) where

import Data.Bits
import Data.Maybe(fromMaybe)

import Seri.Bit
import Seri.Failable
import Seri.Lambda

import Seri.Target.Elaborate.Fresh3

data Mode = WHNF -- ^ elaborate to weak head normal form.
          | SNF  -- ^ elaborate to sharing normal form.
    deriving (Show, Eq, Ord)

data EState = ES_None | ES_Some Mode
    deriving (Show, Eq)

data ExpH = LitEH Lit
          | CaseEH EState ExpH [MatchH]
          | AppEH EState ExpH ExpH
          | LamEH EState Sig (ExpH -> ExpH)
          | ConEH Sig
          | VarEH EState Sig
    deriving(Show)

instance Show (a -> ExpH) where
    show _ = "(function)"


data MatchH = MatchH Pat ([(Name, ExpH)] -> ExpH)
    deriving (Show)




-- | Elaborate an expression under the given mode.
elaborate :: Mode  -- ^ Elaboration mode
          -> Env   -- ^ context under which to evaluate the expression
          -> Exp   -- ^ expression to evaluate
          -> Exp   -- ^ elaborated expression
elaborate mode env exp =
  let -- translate to our HOAS expression representation
      toh :: [(Name, ExpH)] -> Exp -> ExpH
      toh _ (LitE l) = LitEH l
      toh m (CaseE x ms) = 
        let tomh (Match p b) = MatchH p (\bnd -> (toh (bnd ++ m) b))
        in CaseEH ES_None (toh m x) (map tomh ms)
      toh m (AppE a b) = AppEH ES_None (toh m a) (toh m b)
      toh m (LamE s@(Sig n t) b) = LamEH (ES_Some WHNF) s (\x -> toh ((n, x):m) b)
      toh _ (ConE s) = ConEH s
      toh _ (VarE (Sig n t)) | n == name "Seri.Lib.Prelude.valueof" = 
        let [NumT nt, it] = unarrowsT t
        in LamEH (ES_Some SNF) (Sig (name "_") (NumT nt)) $ \_ -> integerEH (nteval nt)
      toh _ (VarE s@(Sig n _)) | n == name "Seri.Lib.Prelude.__prim_eq_Integer" = biniprim s (\a b -> boolEH (a == b))
      toh _ (VarE s@(Sig n _)) | n == name "Seri.Lib.Prelude.__prim_add_Integer" = biniprim s (\a b -> integerEH (a + b))
      toh _ (VarE s@(Sig n _)) | n == name "Seri.Lib.Prelude.__prim_sub_Integer" = biniprim s (\a b -> integerEH (a - b))
      toh _ (VarE s@(Sig n _)) | n == name "Seri.Lib.Prelude.__prim_mul_Integer" = biniprim s (\a b -> integerEH (a * b))
      toh _ (VarE s@(Sig n _)) | n == name "Seri.Lib.Prelude.<" = biniprim s (\a b -> boolEH (a < b))
      toh _ (VarE s@(Sig n _)) | n == name "Seri.Lib.Prelude.>" = biniprim s (\a b -> boolEH (a > b))

      toh _ (v@(VarE (Sig n _))) | n == name "Seri.Lib.Bit.__prim_zeroExtend_Bit" = error $ "toh todo: " ++ pretty v
      toh _ (v@(VarE (Sig n _))) | n == name "Seri.Lib.Bit.__prim_truncate_Bit" = error $ "toh todo: " ++ pretty v
      toh _ (v@(VarE (Sig n _))) | n == name "Seri.Lib.Prelude.__prim_eq_Char" = error $ "toh todo: " ++ pretty v
      toh _ (v@(VarE (Sig n _))) | n == name "Seri.Lib.Bit.__prim_eq_Bit" = error $ "toh todo: " ++ pretty v
      toh _ (v@(VarE (Sig n _))) | n == name "Seri.Lib.Bit.__prim_add_Bit" = error $ "toh todo: " ++ pretty v
      toh _ (v@(VarE (Sig n _))) | n == name "Seri.Lib.Bit.__prim_sub_Bit" = error $ "toh todo: " ++ pretty v
      toh _ (v@(VarE (Sig n _))) | n == name "Seri.Lib.Bit.__prim_mul_Bit" = error $ "toh todo: " ++ pretty v
      toh _ (v@(VarE (Sig n _))) | n == name "Seri.Lib.Bit.__prim_or_Bit" = error $ "toh todo: " ++ pretty v
      toh _ (v@(VarE (Sig n _))) | n == name "Seri.Lib.Bit.__prim_and_Bit" = error $ "toh todo: " ++ pretty v
      toh _ (v@(VarE (Sig n _))) | n == name "Seri.Lib.Bit.__prim_lsh_Bit" = error $ "toh todo: " ++ pretty v
      toh _ (v@(VarE (Sig n _))) | n == name "Seri.Lib.Bit.__prim_rshl_Bit" = error $ "toh todo: " ++ pretty v
      toh _ (VarE (Sig n (NumT nt))) | n == name "Seri.Lib.Prelude.numeric" = ConEH (Sig (name "#" `nappend` name (show (nteval nt))) (NumT nt))

      --toh _ (v@(VarE (Sig "Seri.Lib.Prelude.error" _))) = error $ "toh todo: " ++ pretty v

      toh m (VarE s@(Sig n _)) =
        case (lookup n m) of
            Just v -> v
            Nothing -> VarEH ES_None s
         

      -- elaborate the given expression
      elab :: ExpH -> ExpH
      elab e@(LitEH l) = e
      elab e@(CaseEH (ES_Some m) _ _) | mode <= m = e
      elab (CaseEH (ES_Some WHNF) x ms) =
        let elabm :: MatchH -> MatchH
            elabm (MatchH p f) = MatchH p (\m -> elab (f m))
        in CaseEH (ES_Some SNF) (elab x) (map elabm ms)
      elab (CaseEH _ x ms) =
        let x' = elab x
        in case matches x' ms of
             NoMatched -> error $ "case no match"
             Matched e -> elab e
             UnMatched ms' | mode == WHNF -> CaseEH (ES_Some WHNF) x' ms'
             UnMatched ms' | mode == SNF -> 
                let elabm :: MatchH -> MatchH
                    elabm (MatchH p f) = MatchH p (\m -> elab (f m))
                in CaseEH (ES_Some SNF) x' (map elabm ms')
      elab e@(AppEH (ES_Some m) _ _) | mode <= m = e
      elab (AppEH _ a b) = 
        case (elab a, elab b) of
            (LamEH _ s f, b') -> elab (f b')
            (a', b') -> AppEH (ES_Some mode) a' b'
      elab e@(LamEH (ES_Some m) _ _) | mode <= m = e
      elab (LamEH _ s f) = LamEH (ES_Some mode) s (\x -> elab (f x))
      elab e@(ConEH s) = e
      elab e@(VarEH (ES_Some m) s) | mode <= m = e
      elab e@(VarEH _ s@(Sig n ct)) =
        case (attemptM $ lookupVar env s) of
            Just (pt, ve) -> elab $ toh [] $ assignexp (assignments pt ct) ve
            Nothing -> VarEH (ES_Some SNF) s

      -- Translate back to the normal Exp representation
      toe :: ExpH -> Fresh Exp
      toe (LitEH l) = return (LitE l)
      toe (CaseEH _ x ms) = 
        let toem (MatchH p f) = do
              let sigs = bindingsP p
              sigs' <- mapM fresh sigs
              let rename = zip sigs sigs'
              let p' = repat (zip sigs sigs') p
              b <- toe (f [(n, VarEH ES_None s) | (Sig n _, s) <- rename])
              return (Match p' b)
        in do
            x' <- toe x
            ms' <- mapM toem ms
            return (CaseE x' ms')
      toe (AppEH _ a b) = do
        a' <- toe a
        b' <- toe b
        return (AppE a' b')
      toe (LamEH _ s f) = do
        s' <- fresh s
        b <- toe (f (VarEH ES_None s'))
        return (LamE s' b)
      toe (ConEH s) = return (ConE s)
      toe (VarEH _ s) = return (VarE s)

      exph = toh [] exp
      elabed = elab exph
      done = runFresh (toe elabed) (free' exp)
  in --trace ("elaborate " ++ show mode ++ ": " ++ pretty exp ++ "\nto: " ++ pretty done)
     done


assignexp :: [(Name, Type)] -> Exp -> Exp
assignexp = assign
        
data MatchResult = Failed | Succeeded [(Name, ExpH)] | Unknown
data MatchesResult
 = Matched ExpH
 | UnMatched [MatchH]
 | NoMatched

-- Match an expression against a sequence of alternatives.
matches :: ExpH -> [MatchH] -> MatchesResult
matches _ [] = NoMatched
matches x ms@((MatchH p f):_) =
  case match p x of 
    Failed -> matches x (tail ms)
    Succeeded vs -> Matched (f vs)
    Unknown -> UnMatched ms

-- Match an expression against a pattern.
match :: Pat -> ExpH -> MatchResult
match (ConP _ nm ps) e =
  case unappsEH e of
    ((ConEH (Sig n _)):_) | n /= nm -> Failed
    ((ConEH {}):args) ->
       let mrs = [match p e | (p, e) <- zip ps args]
           join (Succeeded as) (Succeeded bs) = Succeeded (as ++ bs)
           join Failed _ = Failed
           join (Succeeded _) Failed = Failed
           join _ _ = Unknown
       in foldl join (Succeeded []) mrs
    _ -> Unknown
match (LitP l) (LitEH l') | l == l' = Succeeded []
match (LitP {}) (LitEH {}) = Failed
match (VarP (Sig n _)) e = Succeeded [(n, e)]
match (WildP _) _ = Succeeded []
match _ _ = Unknown

repat :: [(Sig, Sig)] -> Pat -> Pat
repat m =
  let rp :: Pat -> Pat
      rp (ConP t n ps) = ConP t n (map rp ps)
      rp (VarP s) = VarP (fromMaybe s (lookup s m))
      rp p@(LitP {}) = p
      rp p@(WildP {}) = p
  in rp

integerEH :: Integer -> ExpH
integerEH = LitEH . IntegerL 

trueEH :: ExpH
trueEH = ConEH (Sig (name "True") (ConT (name "Bool")))

falseEH :: ExpH
falseEH = ConEH (Sig (name "False") (ConT (name "Bool")))

-- | Boolean expression
boolEH :: Bool -> ExpH
boolEH True = trueEH
boolEH False = falseEH

unappsEH :: ExpH -> [ExpH]
unappsEH (AppEH _ a b) = unappsEH a ++ [b]
unappsEH e = [e]

-- Binary integer primitive.
--  s - signature of the primitive
--  f - primitive implementation
biniprim :: Sig -> (Integer -> Integer -> ExpH) -> ExpH
biniprim s f =
  LamEH (ES_Some WHNF) (Sig (name "a") integerT) $ \a ->
    LamEH (ES_Some WHNF) (Sig (name "b") integerT) $ \b ->
      case (a, b) of
         (LitEH (IntegerL ai), LitEH (IntegerL bi)) -> f ai bi
         _ -> AppEH (ES_Some WHNF) (AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) a) b


