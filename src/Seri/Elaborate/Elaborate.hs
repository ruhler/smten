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
module Seri.Elaborate.Elaborate (
    Mode(..), elabwhnf, elaborate,
    ) where

import Debug.Trace

import Data.Bits
import Data.Functor
import Data.List(genericLength, partition)
import Data.Maybe(fromMaybe)
import Data.Monoid

import Seri.Bit
import Seri.Failable
import qualified Seri.HashTable as HT
import Seri.Lambda

import Seri.Elaborate.FreshFast

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

-- | MatchH is a list of patterns a function describing the body of the match.
-- This function takes as input an association list containing a mapping from
-- Sig to expression. The Sigs correspond to the signatures of all the
-- variables bound by all the patterns, in order from left to right of
-- occurence of pattern and variable within the pattern. The expression is the
-- bound value of that.
--
-- For example, the case expression:
--  case Just (Foo 1 4), Foo 2 5 of
--      Just (Foo a b), Foo c d -> a + b + c + d
--  The argument to this matches function would be:
--      [("a", 1), ("b", 4), ("c", 2), ("d", 5)]
data MatchH = MatchH [Pat] ([(Sig, ExpH)] -> ExpH)
    deriving (Eq, Show)

instance Show (a -> ExpH) where
    show _ = "(function)"

instance Eq (a -> ExpH) where
    (==) _ _ = False

instance Typeof ExpH where
    typeof (LitEH l) = typeof l
    typeof (ConEH s) = typeof s
    typeof (VarEH _ s) = typeof s
    typeof (AppEH _ f xs) =
        let fts = unarrowsT (typeof f)
        in case (drop (length xs) fts) of
              [] -> UnknownT
              ts -> arrowsT ts
    typeof (LaceEH _ []) = UnknownT
    typeof (LaceEH _ (MatchH ps b:_)) =
      let bindings = concatMap bindingsP ps
          bt = typeof (b (zip bindings (map (VarEH ES_None) bindings)))
      in arrowsT (map typeof ps ++ [bt])


-- Weak head normal form elaboration
elabwhnf :: Env -> Exp -> Exp
elabwhnf = elaborate WHNF

-- | Elaborate an expression under the given mode.
elaborate :: Mode  -- ^ Elaboration mode
          -> Env   -- ^ context under which to evaluate the expression
          -> Exp   -- ^ expression to evaluate
          -> Exp   -- ^ elaborated expression
elaborate mode env exp =
  let -- translate to our HOAS expression representation
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
      match p@(ConP _ nm ps) e =
        case unappsEH (elab e) of
          ((ConEH (Sig n _)):_) | n /= nm -> Failed
          ((ConEH {}):args) -> matchps ps args
          _ -> Unknown
      match (LitP l) e | LitEH l' <- elab e
        = if l == l' 
            then Succeeded []
            else Failed
      match (VarP s) e = Succeeded [(s, e)]
      match (WildP _) _ = Succeeded []
      match p e = Unknown

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
      elab e@(AppEH _ f ueargs) = 
        let args = if mode == SNF 
                        then map elab ueargs
                        else ueargs
        in case (elab f) of
            f'@(ConEH s) -> AppEH (ES_Some mode) f' (map elab args)
            AppEH _ f largs -> elab (AppEH ES_None f (largs ++ args))
            LaceEH _ ms@(MatchH ps _ : _) | length args > length ps ->
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
                   --     ... -> g) (blah blah blah)
                   --
                   -- As:
                   --  let _a = (blah blah blah)
                   --  case foo of
                   --     ... -> f _a
                   --     ... -> g _a
                   nms = [name ['_', c] | c <- "abcdefghijklmnopqrstuvwxyz"]
                   tys = map typeof rargs
                   pats = [VarP $ Sig n t | (n, t) <- zip nms tys]
                   lam = LaceEH ES_None [MatchH pats $ \m -> 
                           let ams = map (appm (map snd m)) ms
                           in AppEH ES_None (LaceEH ES_None ams) largs
                         ]
               in elab $ AppEH ES_None lam rargs

            LaceEH _ ms@(MatchH ps _ : _) | length args == length ps ->
               case matchms args ms of
                 NoMatched -> error $ "case no match"
                 Matched e -> elab e
                 UnMatched ms' ->
                   let delambdafy :: ExpH -> [ExpH] -> ExpH
                       delambdafy f (AppEH _ (LaceEH _ bms) cargs : fargs) | mode == SNF =
                           let -- Perform a delambdafication.
                               -- Rewrites:
                               --    (blah blah) (case foo of
                               --                   p1 -> m1  
                               --                   p2 -> m2
                               --                   ...) x y z
                               --    
                               -- As:
                               --    let _f = (blah blah)
                               --    in (case foo of
                               --          p1 -> _f m1
                               --          p2 -> _f m2) x y z
                               --
                               -- TODO: this is suspect. For example, if the
                               -- function _f is not strict in the argument, this
                               -- transformation is wrong, because it makes f
                               -- strict in the argument. But, we've already
                               -- elaborated anything we could, so if f is not
                               -- strict in the argument, wouldn't this already go
                               -- away? I'm not sure.
                               rematch :: ExpH -> MatchH -> MatchH 
                               rematch f (MatchH ps b) = MatchH ps $ \m -> AppEH ES_None f [b m]

                               pat = VarP $ Sig (name "_f") (typeof f)
                               lam = LaceEH ES_None [MatchH [pat] $ \m ->
                                        AppEH ES_None (LaceEH ES_None (map (rematch (snd (head m))) bms)) (cargs ++ fargs)
                                        ]
                           in elab $ AppEH ES_None lam [f]
                       delambdafy f args = AppEH (ES_Some mode) f args
                   in delambdafy (LaceEH (ES_Some mode) ms') args
            f' -> AppEH (ES_Some mode) f' args
      elab e@(LaceEH (ES_Some m) _) | mode <= m = e
      elab e@(LaceEH _ ms) | mode == WHNF = e
      elab e@(LaceEH _ ms) = 
        let elabm :: MatchH -> MatchH
            elabm (MatchH p f) = MatchH p (\m -> elab (f m))
        in LaceEH (ES_Some mode) (map elabm ms)

      -- Translate back to the normal Exp representation
      toeM :: ExpH -> Fresh Exp
      toeM (LitEH l) = return (LitE l)
      toeM (ConEH s) = return (ConE s)
      toeM (VarEH _ s) = return (VarE s)
      toeM (AppEH _ f xs) = do
        f' <- toeM f
        xs' <- mapM toeM xs
        return (AppE f' xs')
      toeM (LaceEH _ ms) = 
        let toem (MatchH ps f) = do
              let sigs = concatMap bindingsP ps
              sigs' <- mapM fresh sigs
              let rename = zip sigs sigs'
              let ps' = map (repat (zip sigs sigs')) ps
              b <- toeM (f [(s, VarEH ES_None s') | (s, s') <- rename])
              return (Match ps' b)
        in LaceE <$> mapM toem ms
    
      toe :: ExpH -> Exp
      toe e = runFresh (toeM e) (free' exp)

      -- Binary integer primitive.
      --  s - signature of the primitive
      --  f - primitive implementation
      biniprim :: Sig -> (Integer -> Integer -> ExpH) -> ExpH
      biniprim s f =
        LaceEH (ES_Some WHNF) [
          MatchH [VarP $ Sig (name "a") integerT, VarP $ Sig (name "b") integerT] $ 
            \[(_, a), (_, b)] -> 
               case (elab a, elab b) of
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
               case (elab a, elab b) of
                  (LitEH (CharL av), LitEH (CharL bv)) -> f av bv
                  _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) [a, b]
          ]

      -- Extract a Bit from an expression of the form: __prim_frominteger_Bit v
      -- The expression should be elaborated already.
      unbit :: ExpH -> Maybe Bit
      unbit (AppEH _ (VarEH _ (Sig fib (AppT _ (AppT _ (NumT w))))) [ve])
        | fib == name "Seri.Lib.Bit.__prim_fromInteger_Bit"
        , LitEH (IntegerL v) <- elab ve
        = Just (bv_make (nteval w) v)
      unbit _ = Nothing

   
      -- Binary bitvector primitive.
      --  s - signature of the primitive
      --  f - primitive implementation
      binbprim :: Sig -> (Bit -> Bit -> ExpH) -> ExpH
      binbprim s@(Sig n t) f =
        let [ta, tb, _] = unarrowsT t
        in LaceEH (ES_Some WHNF) [
             MatchH [VarP $ Sig (name "a") ta, VarP $ Sig (name "b") tb] $ 
               \[(_, a), (_, b)] ->
                 case (elab a, elab b) of
                    (a', b') | Just av <- unbit a'
                             , Just bv <- unbit b'
                             -> f av bv
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
                 case (elab a, elab b) of
                    (a', LitEH (IntegerL bv))
                      | Just av <- unbit a'
                      -> f av bv
                    _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) [a, b]
            ]



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
                    case (elab a) of
                      av | av == trueEH -> b
                      av | av == falseEH -> falseEH
                      _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) [a, b]
                ]),
            (name "Seri.Lib.Prelude.||", \s -> 
              LaceEH (ES_Some WHNF) [
                MatchH [VarP $ Sig (name "a") boolT, VarP $ Sig (name "b") boolT] $
                  \[(_, a), (_, b)] ->
                    case (elab a) of
                      av | av == trueEH -> trueEH
                      av | av == falseEH -> b
                      _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) [a, b]
                ]),
            (name "Seri.Lib.Prelude.not", \s -> 
              LaceEH (ES_Some WHNF) [
                MatchH [VarP $ Sig (name "a") boolT] $
                  \[(_, a)] ->
                    case (elab a) of
                      av | av == trueEH -> falseEH
                      av | av == falseEH -> trueEH
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
                       case (elab a) of
                         a' | Just av <- unbit a' -> bitEH $ bv_zero_extend (nteval wt - bv_width av) av
                         _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) [a]
                   ]),
            (name "Seri.Lib.Bit.__prim_truncate_Bit", \s@(Sig _ t) ->
              let [ta, AppT _ (NumT wt)] = unarrowsT t
              in LaceEH (ES_Some WHNF) [
                   MatchH [VarP $ Sig (name "a") ta] $
                     \[(_, a)] ->
                       case (elab a) of
                         a' | Just av <- unbit a' -> bitEH $ bv_truncate (nteval wt) av
                         _ -> AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) [a]
                   ])
            ]


      exph = toh [] exp
      elabed = elab exph
      done = toe elabed
  in --trace ("elab " ++ show mode ++ ": " ++ pretty exp) $
     --trace ("To: " ++ pretty done) $
     done


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

bitEH :: Bit -> ExpH
bitEH b = AppEH (ES_Some SNF) (VarEH (ES_Some SNF) (Sig (name "Seri.Lib.Bit.__prim_fromInteger_Bit") (arrowsT [integerT, bitT (bv_width b)]))) [integerEH $ bv_value b]

