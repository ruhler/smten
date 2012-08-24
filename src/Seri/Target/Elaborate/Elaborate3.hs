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

import Debug.Trace

import Data.Bits
import Data.List(partition)
import Data.Maybe(fromMaybe)

import Seri.Bit
import Seri.Failable
import qualified Seri.HashTable as HT
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
          | LamEH EState VarUse Sig (ExpH -> ExpH)
          | ConEH Sig
          | VarEH EState Sig
    deriving(Show)

instance Show (a -> ExpH) where
    show _ = "(function)"


data MatchH = MatchH Pat [VarUse] ([(Name, ExpH)] -> ExpH)
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
        let tomh (Match p b) = MatchH p [varuse n b | n <- bindingsP' p] (\bnd -> (toh (bnd ++ m) b))
        in CaseEH ES_None (toh m x) (map tomh ms)
      toh m (AppE a b) = AppEH ES_None (toh m a) (toh m b)
      toh m (LamE s@(Sig n t) b) = LamEH (ES_Some WHNF) (varuse n b) s (\x -> toh ((n, x):m) b)
      toh _ (ConE s) = ConEH s
      toh m (VarE s@(Sig n _)) =
        case (HT.lookup n primitives) of
            Just f -> f s
            Nothing ->
              case (lookup n m) of
                  Just v -> v
                  Nothing -> VarEH ES_None s
         
      dontshare = True
     
      shouldReduce :: VarUse -> ExpH -> Bool
      shouldReduce vu e | dontshare = True
      shouldReduce vu e = shouldReduce' False vu e

      shouldReduce' :: Bool -> VarUse -> ExpH -> Bool
      shouldReduce' _ _ (LitEH {}) = True
      shouldReduce' _ _ (LamEH {}) = True
      shouldReduce' _ _ (ConEH {}) = True
      shouldReduce' False _ (VarEH {}) = True
      shouldReduce' _ VU_None _ = True
      shouldReduce' _ VU_Single _ = True
      shouldReduce' _ _ (CaseEH {}) = False
      shouldReduce' _ vu (AppEH _ a b) = shouldReduce' True vu a && shouldReduce' True vu b
      shouldReduce' True _ (VarEH _ s) =
        case (attemptM $ lookupVarInfo env s) of
            Just _ -> True
            Nothing -> False

      -- Match an expression against a sequence of alternatives.
      matches :: ExpH -> [MatchH] -> MatchesResult
      matches _ [] = NoMatched
      matches x ms@((MatchH p vus f):_) =
        case match p x of 
          Failed -> matches x (tail ms)
          Succeeded vs -> 
            let vs' = zip vus vs
                (red, nored) = partition (\(vu, (_, b)) -> shouldReduce vu b) vs'
                letEH :: [(VarUse, (Sig, ExpH))] -> [(Sig, ExpH)] -> ([(Name, ExpH)] -> ExpH) -> ExpH
                letEH [] rs f = elab (f [(n, e) | (Sig n _, e) <- rs])
                letEH ((vu, (s, v)):bs) rs f = AppEH ES_None (LamEH (ES_Some mode) vu s (\x -> letEH bs ((s, x):rs) f)) v
            in Matched $ letEH nored (map snd red) f
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
      match (VarP s) e = Succeeded [(s, e)]
      match (WildP _) _ = Succeeded []
      match _ _ = Unknown


      -- elaborate the given expression
      elab :: ExpH -> ExpH
      elab e@(LitEH l) = e
      elab e@(CaseEH (ES_Some m) _ _) | mode <= m = e
      elab (CaseEH (ES_Some WHNF) x ms) =
        let elabm :: MatchH -> MatchH
            elabm (MatchH p vus f) = MatchH p vus (\m -> elab (f m))
        in CaseEH (ES_Some SNF) (elab x) (map elabm ms)
      elab (CaseEH _ x ms) =
        let x' = elab x
        in case matches x' ms of
             NoMatched -> error $ "case no match"
             Matched e -> elab e
             UnMatched ms' | mode == WHNF -> CaseEH (ES_Some WHNF) x' ms'
             UnMatched ms' | mode == SNF -> 
                let elabm :: MatchH -> MatchH
                    elabm (MatchH p vus f) = MatchH p vus (\m -> elab (f m))
                in CaseEH (ES_Some SNF) x' (map elabm ms')
      elab e@(AppEH (ES_Some m) _ _) | mode <= m = e
      elab (AppEH _ a b) = 
        case (elab a, elab b) of
            (LamEH _ vu s f, b') | shouldReduce vu b' -> elab (f b')
            (a', b') -> AppEH (ES_Some mode) a' b'
      elab e@(LamEH (ES_Some m) _ _ _) | mode <= m = e
      elab (LamEH _ vu s f) = LamEH (ES_Some mode) vu s (\x -> elab (f x))
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
        let toem (MatchH p _ f) = do
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
      toe (LamEH _ _ s f) = do
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
        
data MatchResult = Failed | Succeeded [(Sig, ExpH)] | Unknown
data MatchesResult
 = Matched ExpH
 | UnMatched [MatchH]
 | NoMatched

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

data VarUse = VU_None | VU_Single | VU_Multi
    deriving (Eq, Show)

joinuse :: VarUse -> VarUse -> VarUse
joinuse a b =
    case a of
        VU_None -> b
        VU_Single -> 
            case b of
                VU_None -> VU_Single
                _ -> VU_Multi
        VU_Multi -> VU_Multi

varuse :: Name -> Exp -> VarUse
varuse n =
  let vu :: Exp -> VarUse
      vu (LitE {}) = VU_None
      vu (CaseE x ms) =
        let vum :: Match -> VarUse
            vum (Match p _) | n `elem` bindingsP' p = VU_None
            vum (Match _ b) = vu b
        in foldr joinuse (vu x) (map vum ms)
      vu (AppE a b) = joinuse (vu a) (vu b)
      vu (LamE (Sig nm _) _) | nm == n = VU_None
      vu (LamE _ b) = vu b
      vu (ConE {}) = VU_None
      vu (VarE (Sig nm _)) | nm == n = VU_Single
      vu (VarE {}) = VU_None
  in vu


primitives :: HT.HashTable Name (Sig -> ExpH)
primitives = HT.table $ [
      (name "Seri.Lib.Prelude.__prim_eq_Integer", \s -> biniprim s (\a b -> boolEH (a == b))),
      (name "Seri.Lib.Prelude.__prim_add_Integer", \s -> biniprim s (\a b -> integerEH (a + b))),
      (name "Seri.Lib.Prelude.__prim_sub_Integer", \s -> biniprim s (\a b -> integerEH (a - b))),
      (name "Seri.Lib.Prelude.__prim_mul_Integer", \s -> biniprim s (\a b -> integerEH (a * b))),
      (name "Seri.Lib.Prelude.<", \s -> biniprim s (\a b -> boolEH (a < b))),
      (name "Seri.Lib.Prelude.>", \s -> biniprim s (\a b -> boolEH (a > b))),

      (name "Seri.Lib.Prelude.valueof", \(Sig n t) ->
        let [NumT nt, it] = unarrowsT t
        in LamEH (ES_Some SNF) VU_None (Sig (name "_") (NumT nt)) $ \_ -> integerEH (nteval nt)),

      (name "Seri.Lib.Prelude.numeric", \(Sig _ (NumT nt)) -> ConEH (Sig (name "#" `nappend` name (show (nteval nt))) (NumT nt))),

      (name "Seri.Lib.Bit.__prim_zeroExtend_Bit", \s@(Sig _ t) ->
        let [ta, AppT _ (NumT wt)] = unarrowsT t
        in LamEH (ES_Some WHNF) VU_Single (Sig (name "a") ta) $ \a ->
            case (unbit a) of
              Just av -> bitEH $ bv_zero_extend (nteval wt - bv_width av) av
              _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) a
        ),
      (name "Seri.Lib.Bit.__prim_truncate_Bit", \s@(Sig _ t) ->
        let [ta, AppT _ (NumT wt)] = unarrowsT t
        in LamEH (ES_Some WHNF) VU_Single (Sig (name "a") ta) $ \a ->
            case (unbit a) of
              Just av -> bitEH $ bv_truncate (nteval wt) av
              _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) a
        ),
      (name "Seri.Lib.Prelude.__prim_eq_Char", \s -> bincprim s (\a b -> boolEH (a == b))),
      (name "Seri.Lib.Bit.__prim_eq_Bit", \s -> binbprim s (\a b -> boolEH (a == b))),
      (name "Seri.Lib.Bit.__prim_add_Bit", \s -> binbprim s (\a b -> bitEH (a + b))),
      (name "Seri.Lib.Bit.__prim_sub_Bit", \s -> binbprim s (\a b -> bitEH (a - b))),
      (name "Seri.Lib.Bit.__prim_mul_Bit", \s -> binbprim s (\a b -> bitEH (a * b))),
      (name "Seri.Lib.Bit.__prim_or_Bit", \s -> binbprim s (\a b -> bitEH (a .|. b))),
      (name "Seri.Lib.Bit.__prim_and_Bit", \s -> binbprim s (\a b -> bitEH (a .&. b))),
      (name "Seri.Lib.Bit.__prim_lsh_Bit", \s -> binbiprim s (\a b -> bitEH (a `shiftL` fromInteger b))),
      (name "Seri.Lib.Bit.__prim_rshl_Bit", \s -> binbiprim s (\a b -> bitEH (a `shiftR` fromInteger b)))
   ] 

unbit :: ExpH -> Maybe Bit
unbit (AppEH _ (VarEH _ (Sig fib (AppT _ (AppT _ (NumT w))))) (LitEH (IntegerL v))) | fib == name "Seri.Lib.Bit.__prim_fromInteger_Bit"
 = Just (bv_make (nteval w) v)
unbit _ = Nothing

bitEH :: Bit -> ExpH
bitEH b = AppEH (ES_Some SNF) (VarEH (ES_Some SNF) (Sig (name "Seri.Lib.Bit.__prim_fromInteger_Bit") (arrowsT [integerT, bitT (bv_width b)]))) (integerEH $ bv_value b)

-- Binary integer primitive.
--  s - signature of the primitive
--  f - primitive implementation
biniprim :: Sig -> (Integer -> Integer -> ExpH) -> ExpH
biniprim s f =
  LamEH (ES_Some WHNF) VU_Single (Sig (name "a") integerT) $ \a ->
    LamEH (ES_Some WHNF) VU_Single (Sig (name "b") integerT) $ \b ->
      case (a, b) of
         (LitEH (IntegerL ai), LitEH (IntegerL bi)) -> f ai bi
         _ -> AppEH (ES_Some WHNF) (AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) a) b

-- Binary character primitive.
--  s - signature of the primitive
--  f - primitive implementation
bincprim :: Sig -> (Char -> Char -> ExpH) -> ExpH
bincprim s f =
  LamEH (ES_Some WHNF) VU_Single (Sig (name "a") charT) $ \a ->
    LamEH (ES_Some WHNF) VU_Single (Sig (name "b") charT) $ \b ->
      case (a, b) of
         (LitEH (CharL av), LitEH (CharL bv)) -> f av bv
         _ -> AppEH (ES_Some WHNF) (AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) a) b


-- Binary bitvector primitive.
--  s - signature of the primitive
--  f - primitive implementation
binbprim :: Sig -> (Bit -> Bit -> ExpH) -> ExpH
binbprim s@(Sig n t) f =
  let [ta, tb, _] = unarrowsT t
  in LamEH (ES_Some WHNF) VU_Single (Sig (name "a") ta) $ \a ->
       LamEH (ES_Some WHNF) VU_Single (Sig (name "b") tb) $ \b ->
         case (unbit a, unbit b) of
            (Just av, Just bv) -> f av bv
            _ -> AppEH (ES_Some WHNF) (AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) a) b

-- Binary bitvector/integer primitive.
--  s - signature of the primitive
--  f - primitive implementation
binbiprim :: Sig -> (Bit -> Integer -> ExpH) -> ExpH
binbiprim s@(Sig n t) f =
  let [ta, tb, _] = unarrowsT t
  in LamEH (ES_Some WHNF) VU_Single (Sig (name "a") ta) $ \a ->
       LamEH (ES_Some WHNF) VU_Single (Sig (name "b") tb) $ \b ->
         case (unbit a, b) of
            (Just av, LitEH (IntegerL bv)) -> f av bv
            _ -> AppEH (ES_Some WHNF) (AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) a) b
