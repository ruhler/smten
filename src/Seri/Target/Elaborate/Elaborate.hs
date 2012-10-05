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
{-# LANGUAGE PatternGuards #-}

-- | Target for elaborating seri expressions.
module Seri.Target.Elaborate.Elaborate (
    Mode(..), elabwhnf, elaborate,
    ) where

import Debug.Trace

import Data.Bits
import Data.Functor
import Data.Maybe(fromMaybe)
import Data.Monoid

import Seri.Bit
import Seri.Failable
import qualified Seri.HashTable as HT
import Seri.Lambda

import Seri.Target.Elaborate.FreshPretty

data Mode = WHNF -- ^ elaborate to weak head normal form.
          | SNF  -- ^ elaborate to sharing normal form.
    deriving (Show, Eq, Ord)

data EState = ES_None | ES_Some Mode
    deriving (Show, Eq)

data ExpH = LitEH Lit
          | ConEH Sig
          | VarEH EState Sig
          | AppEH EState ExpH [ExpH]
          | LaceEH EState [MatchH]
    deriving(Eq, Show)

data MatchH = MatchH [Pat] ([(Sig, ExpH)] -> ExpH)
    deriving (Eq, Show)

instance Show (a -> ExpH) where
    show _ = "(function)"

instance Eq (a -> ExpH) where
    (==) _ _ = False


-- Weak head normal form elaboration
elabwhnf :: Env -> Exp -> Exp
elabwhnf = elaborate WHNF

-- | Elaborate an expression under the given mode.
elaborate :: Mode  -- ^ Elaboration mode
          -> Env   -- ^ context under which to evaluate the expression
          -> Exp   -- ^ expression to evaluate
          -> Exp   -- ^ elaborated expression
elaborate mode env exp =
  let -- Flag specifying if we should delambdafy or not.
      -- TODO: make this a parameter to the elaborator?
      delambdafy = True

      -- translate to our HOAS expression representation
      toh :: [(Sig, ExpH)] -> Exp -> ExpH
      toh _ (LitE l) = LitEH l
      toh _ (ConE s) = ConEH s
      toh m (VarE s@(Sig n _)) | Just f <- HT.lookup n primitives = f s
      toh m (VarE s) | Just v <- lookup s m = v
      toh m (VarE s) = VarEH ES_None s
      toh m (AppE f xs) = AppEH ES_None (toh m f) (map (toh m) xs)
      toh m (LaceE ms) = 
        let tomh (Match ps b) = MatchH ps (\bnd -> (toh (bnd ++ m) b))
        in LaceEH ES_None (map tomh ms)
         
      -- Match expressions against a sequence of alternatives.
      matchms :: [ExpH] -> [MatchH] -> MatchesResult
      matchms _ [] = NoMatched
      matchms xs ms@((MatchH ps f):_) =
        case matchps ps xs of 
          Failed -> matchms xs (tail ms)
          Succeeded vs -> Matched $ f vs
          Unknown -> UnMatched ms
     
      -- Match expressions against patterns.
      matchps :: [Pat] -> [ExpH] -> MatchResult
      matchps ps args = mconcat [match p e | (p, e) <- zip ps args]

      match :: Pat -> ExpH -> MatchResult
      match (ConP _ nm ps) e =
        case unappsEH e of
          ((ConEH (Sig n _)):_) | n /= nm -> Failed
          ((ConEH {}):args) -> matchps ps args
          _ -> Unknown
      match (LitP l) (LitEH l') | l == l' = Succeeded []
      match (LitP {}) (LitEH {}) = Failed
      match (VarP s) e = Succeeded [(s, e)]
      match (WildP _) _ = Succeeded []
      match _ _ = Unknown

      -- elaborate the given expression
      elab :: ExpH -> ExpH
      elab e@(LitEH l) = e
      elab e@(ConEH s) = e
      elab e@(VarEH (ES_Some m) s) | mode <= m = e
      elab e@(VarEH _ s@(Sig n ct)) =
        case (attemptM $ lookupVar env s) of
            Just (pt, ve) -> elab $ toh [] $ assignexp (assignments pt ct) ve
            Nothing -> VarEH (ES_Some SNF) s
      elab (AppEH _ x []) = elab x
      elab e@(AppEH (ES_Some m) _ _) | mode <= m = e
      elab e@(AppEH _ f xs) = 
        case (elab f, map elab xs) of
            (AppEH _ f largs, rargs) -> elab (AppEH ES_None f (largs ++ rargs))
            (LaceEH _ ms@(MatchH ps _ : _), args) | length args >= length ps ->
               let -- Apply the given arguments to the body of the match.
                   appm :: [ExpH] -> MatchH -> MatchH
                   appm [] m = m
                   appm xs (MatchH p f) = MatchH p (\m -> AppEH ES_None (f m) xs)

                   (largs, rargs) = splitAt (length ps) args

                   -- Push all extra arguments into the matches.
                   -- This serves two purposes. It applies the extra arguments
                   -- to which ever alternative matches, and it performs a
                   -- delambdafication in case there are no matches.
                   -- That is, it rewrites:
                   --  (case foo of
                   --     ... -> f
                   --     ... -> g) x
                   --
                   -- As:
                   --  case foo of
                   --     ... -> f x
                   --     ... -> g x
                   ams = map (appm rargs) ms
               in case matchms largs ams of
                    NoMatched -> error $ "case no match"
                    Matched e -> elab e
                    UnMatched ms' ->
                      case largs of
                        (AppEH _ (LaceEH _ bms) largs : rargs) | mode == SNF, delambdafy ->
                          let -- Perform a delambdafication.
                              -- Rewrites:
                              --    case (case foo of
                              --             p1 -> m1  
                              --             p2 -> m2
                              --             ...) of
                              --       P1 -> M1
                              --       P2 -> M2
                              --       ...
                              --    
                              -- As:
                              --    case foo of
                              --       p1 -> case m1 of
                              --               P1 -> M1
                              --               P2 -> M2
                              --               ...
                              --       p2 -> case m2 of
                              --               P1 -> M1
                              --               P2 -> M2
                              --               ...
                              (lrargs, rrargs) = splitAt (length ps) rargs
                              rematch :: MatchH -> MatchH 
                              rematch (MatchH ps f) = MatchH ps $ \m -> AppEH ES_None (LaceEH ES_None ms') (f m : lrargs)
                          in elab $ AppEH ES_None (LaceEH ES_None (map rematch bms)) (largs ++ rrargs)
                        _ -> AppEH (ES_Some mode) (LaceEH (ES_Some mode) ms') largs
            (a', b') -> AppEH (ES_Some mode) a' b'
      elab e@(LaceEH (ES_Some m) _) | mode <= m = e
      elab e@(LaceEH _ ms) | mode == WHNF = e
      elab (LaceEH _ ms) =
        let elabm :: MatchH -> MatchH
            elabm (MatchH p f) = MatchH p (\m -> elab (f m))
        in LaceEH (ES_Some mode) (map elabm ms)

      -- Translate back to the normal Exp representation
      toe :: ExpH -> Fresh Exp
      toe (LitEH l) = return (LitE l)
      toe (ConEH s) = return (ConE s)
      toe (VarEH _ s) = return (VarE s)
      toe (AppEH _ f xs) = do
        f' <- toe f
        xs' <- mapM toe xs
        return (AppE f' xs')
      toe (LaceEH _ ms) = 
        let toem (MatchH ps f) = do
              let sigs = concatMap bindingsP ps
              sigs' <- mapM fresh sigs
              let rename = zip sigs sigs'
              let ps' = map (repat (zip sigs sigs')) ps
              b <- toe (f [(s, VarEH ES_None s') | (s, s') <- rename])
              return (Match ps' b)
        in LaceE <$> mapM toem ms

      exph = toh [] exp
      elabed = elab exph
      done = runFresh (toe elabed) (free' exp)
  in done


assignexp :: [(Name, Type)] -> Exp -> Exp
assignexp = assign
        
data MatchResult = Failed | Succeeded [(Sig, ExpH)] | Unknown
data MatchesResult
 = Matched ExpH
 | UnMatched [MatchH]
 | NoMatched

instance Monoid MatchResult where
   mempty = Succeeded []
   mappend (Succeeded as) (Succeeded bs) = Succeeded (as ++ bs)
   mappend Failed _ = Failed
   mappend (Succeeded _) Failed = Failed
   mappend _ _ = Unknown

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
unappsEH (AppEH _ a xs) = unappsEH a ++ xs
unappsEH e = [e]

primitives :: HT.HashTable Name (Sig -> ExpH)
primitives = HT.table $ [
      (name "Seri.Lib.Prelude.__prim_eq_Integer", \s -> biniprim s (\a b -> boolEH (a == b))),
      (name "Seri.Lib.Prelude.__prim_add_Integer", \s -> biniprim s (\a b -> integerEH (a + b))),
      (name "Seri.Lib.Prelude.__prim_sub_Integer", \s -> biniprim s (\a b -> integerEH (a - b))),
      (name "Seri.Lib.Prelude.__prim_mul_Integer", \s -> biniprim s (\a b -> integerEH (a * b))),
      (name "Seri.Lib.Prelude.<", \s -> biniprim s (\a b -> boolEH (a < b))),
      (name "Seri.Lib.Prelude.<=", \s -> biniprim s (\a b -> boolEH (a <= b))),
      (name "Seri.Lib.Prelude.>", \s -> biniprim s (\a b -> boolEH (a > b))),
      (name "Seri.Lib.Prelude.__prim_eq_Char", \s -> bincprim s (\a b -> boolEH (a == b))),
      (name "Seri.Lib.Bit.__prim_eq_Bit", \s -> binbprim s (\a b -> boolEH (a == b))),
      (name "Seri.Lib.Bit.__prim_add_Bit", \s -> binbprim s (\a b -> bitEH (a + b))),
      (name "Seri.Lib.Bit.__prim_sub_Bit", \s -> binbprim s (\a b -> bitEH (a - b))),
      (name "Seri.Lib.Bit.__prim_mul_Bit", \s -> binbprim s (\a b -> bitEH (a * b))),
      (name "Seri.Lib.Bit.__prim_or_Bit", \s -> binbprim s (\a b -> bitEH (a .|. b))),
      (name "Seri.Lib.Bit.__prim_and_Bit", \s -> binbprim s (\a b -> bitEH (a .&. b))),
      (name "Seri.Lib.Bit.__prim_lsh_Bit", \s -> binbiprim s (\a b -> bitEH (a `shiftL` fromInteger b))),
      (name "Seri.Lib.Bit.__prim_rshl_Bit", \s -> binbiprim s (\a b -> bitEH (a `shiftR` fromInteger b))),
      (name "Seri.Lib.Bit.__prim_extract_Bit", \s@(Sig _ t) ->
         binbiprim s (\a j ->
            let AppT _ (NumT wt) = last $ unarrowsT t
                i = j + (nteval wt) - 1
            in bitEH (bv_extract i j a))),
      (name "Seri.Lib.Prelude.&&", \s -> 
        LaceEH (ES_Some WHNF) [
          MatchH [VarP $ Sig (name "a") boolT, VarP $ Sig (name "b") boolT] $
            \[(_, a), (_, b)] ->
              case () of
                _ | a == trueEH -> b
                _ | a == falseEH -> falseEH
                _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) [a, b]
          ]),
      (name "Seri.Lib.Prelude.||", \s -> 
        LaceEH (ES_Some WHNF) [
          MatchH [VarP $ Sig (name "a") boolT, VarP $ Sig (name "b") boolT] $
            \[(_, a), (_, b)] ->
              case () of
                _ | a == trueEH -> trueEH
                _ | a == falseEH -> b
                _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) [a, b]
          ]),
      (name "Seri.Lib.Prelude.not", \s -> 
        LaceEH (ES_Some WHNF) [
          MatchH [VarP $ Sig (name "a") boolT] $
            \[(_, a)] ->
              case () of
                _ | a == trueEH -> falseEH
                _ | a == falseEH -> trueEH
                _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) [a]
          ]),
      (name "Seri.Lib.Prelude.valueof", \(Sig n t) ->
        let [NumT nt, it] = unarrowsT t
        in LaceEH (ES_Some SNF) [
            MatchH [VarP $ Sig (name "_") (NumT nt)] $
              \_ -> integerEH (nteval nt)
            ]),
      (name "Seri.Lib.Prelude.numeric", \(Sig _ (NumT nt)) -> ConEH (Sig (name "#" `nappend` name (show (nteval nt))) (NumT nt))),
      (name "Seri.Lib.Bit.__prim_zeroExtend_Bit", \s@(Sig _ t) ->
        let [ta, AppT _ (NumT wt)] = unarrowsT t
        in LaceEH (ES_Some WHNF) [
             MatchH [VarP $ Sig (name "a") ta] $
               \[(_, a)] ->
                 case (unbit a) of
                   Just av -> bitEH $ bv_zero_extend (nteval wt - bv_width av) av
                   _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) [a]
             ]),
      (name "Seri.Lib.Bit.__prim_truncate_Bit", \s@(Sig _ t) ->
        let [ta, AppT _ (NumT wt)] = unarrowsT t
        in LaceEH (ES_Some WHNF) [
             MatchH [VarP $ Sig (name "a") ta] $
               \[(_, a)] ->
                 case (unbit a) of
                   Just av -> bitEH $ bv_truncate (nteval wt) av
                   _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) [a]
             ])
      ]

unbit :: ExpH -> Maybe Bit
unbit (AppEH _ (VarEH _ (Sig fib (AppT _ (AppT _ (NumT w))))) [LitEH (IntegerL v)]) | fib == name "Seri.Lib.Bit.__prim_fromInteger_Bit"
 = Just (bv_make (nteval w) v)
unbit _ = Nothing

bitEH :: Bit -> ExpH
bitEH b = AppEH (ES_Some SNF) (VarEH (ES_Some SNF) (Sig (name "Seri.Lib.Bit.__prim_fromInteger_Bit") (arrowsT [integerT, bitT (bv_width b)]))) [integerEH $ bv_value b]

-- Binary integer primitive.
--  s - signature of the primitive
--  f - primitive implementation
biniprim :: Sig -> (Integer -> Integer -> ExpH) -> ExpH
biniprim s f =
  LaceEH (ES_Some WHNF) [
    MatchH [VarP $ Sig (name "a") integerT, VarP $ Sig (name "b") integerT] $ 
      \[(_, a), (_, b)] -> 
         case (a, b) of
            (LitEH (IntegerL ai), LitEH (IntegerL bi)) -> f ai bi
            _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) [a, b]
    ]

-- Binary character primitive.
--  s - signature of the primitive
--  f - primitive implementation
bincprim :: Sig -> (Char -> Char -> ExpH) -> ExpH
bincprim s f =
  LaceEH (ES_Some WHNF) [
    MatchH [VarP $ Sig (name "a") charT, VarP $ Sig (name "b") charT] $
      \[(_, a), (_, b)] ->
         case (a, b) of
            (LitEH (CharL av), LitEH (CharL bv)) -> f av bv
            _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) [a, b]
    ]

-- Binary bitvector primitive.
--  s - signature of the primitive
--  f - primitive implementation
binbprim :: Sig -> (Bit -> Bit -> ExpH) -> ExpH
binbprim s@(Sig n t) f =
  let [ta, tb, _] = unarrowsT t
  in LaceEH (ES_Some WHNF) [
       MatchH [VarP $ Sig (name "a") ta, VarP $ Sig (name "b") tb] $ 
         \[(_, a), (_, b)] ->
           case (unbit a, unbit b) of
              (Just av, Just bv) -> f av bv
              _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) [a, b]
       ]

-- Binary bitvector/integer primitive.
--  s - signature of the primitive
--  f - primitive implementation
binbiprim :: Sig -> (Bit -> Integer -> ExpH) -> ExpH
binbiprim s@(Sig n t) f =
  let [ta, tb, _] = unarrowsT t
  in LaceEH (ES_Some WHNF) [
      MatchH [VarP $ Sig (name "a") ta, VarP $ Sig (name "b") tb] $
        \[(_, a), (_, b)] ->
           case (unbit a, b) of
              (Just av, LitEH (IntegerL bv)) -> f av bv
              _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) [a, b]
      ]
