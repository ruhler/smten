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
import Seri.Lambda.Ppr hiding (Mode)

import Seri.Elaborate.FreshPretty

data Mode = WHNF -- ^ elaborate to weak head normal form.
          | SNF  -- ^ elaborate to sharing normal form.
    deriving (Show, Eq, Ord)

data EState = ES_None | ES_Some Mode
    deriving (Show, Eq)

data ExpH = LitEH Lit
          | ConEH Sig
          | VarEH EState Sig
          | AppEH EState ExpH ExpH
          | LaceEH EState [MatchH]
    deriving(Eq)

instance Ppr ExpH where
    ppr (LitEH l) = ppr l
    ppr (ConEH s) = ppr s
    ppr (VarEH _ s) = ppr s
    ppr (AppEH _ f x) = parens (ppr f) <+> parens (ppr x)
    ppr (LaceEH _ ms)
        = text "case" <+> text "of" <+> text "{"
            $+$ nest tabwidth (vcat (map ppr ms)) $+$ text "}"
    

-- | MatchH is a list of patterns a function describing the body of the match.
-- This function takes as input an association list containing a mapping from
-- Sig to expression. The Sigs correspond to the signatures of all the
-- variables bound by all the pattern, in order from left to right of
-- the variable within the pattern. The expression is the bound value of that.
--
-- For example, the case expression:
--  case Just (Foo 1 4) of
--      Just (Foo a b) -> a + b + c + d
--  The argument to this matches function would be:
--      [("a", 1), ("b", 4)]
data MatchH = MatchH Pat ([(Sig, ExpH)] -> ExpH)
    deriving (Eq)

instance Ppr MatchH where
    ppr (MatchH p _) = ppr p <+> text "->" <+> text "..." Seri.Lambda.Ppr.<> semi

instance Eq (a -> ExpH) where
    (==) _ _ = False

instance Typeof ExpH where
    typeof (LitEH l) = typeof l
    typeof (ConEH s) = typeof s
    typeof (VarEH _ s) = typeof s
    typeof (AppEH _ f x) =
        let fts = unarrowsT (typeof f)
        in case (drop 1 fts) of
              [] -> UnknownT
              ts -> arrowsT ts
    typeof (LaceEH _ []) = UnknownT
    typeof (LaceEH _ (MatchH p b:_)) =
      let bindings = bindingsP p
          bt = typeof (b (zip bindings (map (VarEH ES_None) bindings)))
      in arrowsT [typeof p, bt]


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
      toh m (AppE f xs) =
        let appeh :: ExpH -> Exp -> ExpH
            appeh f x = AppEH ES_None f (toh m x)
        in foldl appeh (toh m f) xs
      toh m (LaceE ms@(Match (_:_:_) _ : _)) = toh m (sLaceE ms)
      toh m (LaceE ms) = 
        let tomh (Match [p] b) = MatchH p (\bnd -> (toh (bnd ++ m) b))
        in LaceEH ES_None (map tomh ms)
         
      -- Match an expression against a sequence of alternatives.
      matchms :: ExpH -> [MatchH] -> MatchesResult
      matchms _ [] = NoMatched
      matchms x ms@((MatchH p f):_) =
        case match p x of 
          Failed -> matchms x (tail ms)
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

      elab :: ExpH -> ExpH
      elab e =
        case elab' e of
            AppEH _ f x | mode == SNF
                        , AppEH _ (LaceEH _ ms) y <- elab x -> 
               let -- perform "function" pushing.
                   -- Rewrites:
                   --    (blah blah) (case y of {p1 -> m1; p2 -> m2})
                   -- As:
                   --    let _f = (blah blah)
                   --    in case y of {p1 -> _f m1; p2 -> _f m2}
                   rematch :: ExpH -> MatchH -> MatchH
                   rematch f (MatchH p b) = MatchH p $ \m -> AppEH ES_None f (b m)

                   pat = VarP $ Sig (name "_f") (typeof f)
                   lam = LaceEH ES_None [MatchH pat $ \[(_, _f)] ->
                            AppEH ES_None (LaceEH ES_None (map (rematch _f) ms)) y 
                          ]
               in elab $ AppEH ES_None lam f
            ee -> ee

      -- elaborate the given expression
      elab' :: ExpH -> ExpH
      elab' e@(LitEH l) = e
      elab' e@(ConEH s) = e
      elab' e@(VarEH (ES_Some m) s) | mode <= m = e
      elab' e@(VarEH _ s@(Sig n ct)) =
        case (attemptM $ lookupVar env s) of
            Just (pt, ve) -> elab $ toh [] $ assignexp (assignments pt ct) ve
            Nothing -> VarEH (ES_Some SNF) s
      elab' e@(AppEH (ES_Some m) _ _) | mode <= m = e
      elab' e@(AppEH _ f arg) = 
           case (elab f) of
            l@(LaceEH _ ms@(MatchH p _ : _)) ->
               case matchms arg ms of
                 NoMatched -> error $ "case no match: " ++ pretty l ++ ",\n " ++ "(" ++ pretty arg ++ ") "
                 Matched e -> elab e
                 UnMatched ms' -> AppEH (ES_Some mode) (LaceEH (ES_Some mode) ms') (if mode == SNF then elab arg else arg)
            (AppEH _ (LaceEH _ ms) y) | mode == SNF ->
                let -- perform "argument pushing"
                    -- Rewrites:
                    --    (case y of { ... -> f; ... -> g) (blah blah)
                    -- As:
                    --    let _a = (blah blah)
                    --    in case y of { ... -> f _a; ... -> g _a }
                    argvar = Sig (name "_a") (typeof arg)

                    appm :: ExpH -> MatchH -> MatchH
                    appm x (MatchH p f) = MatchH p (\m -> AppEH ES_None (f m) x)

                    -- \a -> case y of { ... -> f a; ... -> g a}
                    lam = LaceEH ES_None [MatchH (VarP argvar) $ \[(_, a)] ->
                              let ams = map (appm a) ms
                              in AppEH ES_None (LaceEH ES_None ams) y
                            ]
                in elab $ AppEH ES_None lam arg
            f' -> AppEH (ES_Some mode) f' (if mode == SNF then elab arg else arg)
      elab' e@(LaceEH (ES_Some m) _) | mode <= m = e
      elab' e@(LaceEH _ ms) | mode == WHNF = e
      elab' e@(LaceEH _ ms) = 
        let elabm :: MatchH -> MatchH
            elabm (MatchH p f) = MatchH p (\m -> elab (f m))
        in LaceEH (ES_Some mode) (map elabm ms)

      -- Translate back to the normal Exp representation
      toeM :: ExpH -> Fresh Exp
      toeM (LitEH l) = return (LitE l)
      toeM (ConEH s) = return (ConE s)
      toeM (VarEH _ s) = return (VarE s)
      toeM e@(AppEH {}) = do
        (f:args) <- mapM toeM (unappsEH e)
        return (AppE f args)
      toeM (LaceEH _ ms) = 
        let toem (MatchH p f) = do
              let sigs = bindingsP p
              sigs' <- mapM fresh sigs
              let rename = zip sigs sigs'
              let p' = (repat (zip sigs sigs')) p
              b <- toeM (f [(s, VarEH ES_None s') | (s, s') <- rename])
              return (Match [p'] b)
        in LaceE <$> mapM toem ms
    
      toe :: ExpH -> Exp
      toe e = runFresh (toeM e) (free' exp)

      -- Unary primitive handling
      unary :: (ExpH -> Maybe ExpH) -> Sig -> ExpH
      unary f s@(Sig _ t) =
        let [ta, _] = unarrowsT t
        in LaceEH (ES_Some WHNF) [
             MatchH (VarP $ Sig (name "a") ta) $ 
               \[(_, a)] ->
                  let def = AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) a
                  in fromMaybe def (f (elab a))
             ]

      uXX :: (ExpH -> Maybe a)
             -> (b -> ExpH)
             -> (a -> b)
             -> (Sig -> ExpH)
      uXX de_a mkb f =
        let g a = do
                a' <- de_a a
                return (mkb (f a'))
        in unary g

      uIS :: (Integer -> String) -> (Sig -> ExpH)
      uIS = uXX de_integerEH stringEH

      uVS :: (Bit -> String) -> (Sig -> ExpH)
      uVS = uXX de_bitEH stringEH

      uVV :: (Bit -> Bit) -> (Sig -> ExpH)
      uVV = uXX de_bitEH bitEH

      uBB :: (Bool -> Bool) -> (Sig -> ExpH)
      uBB = uXX de_boolEH boolEH

      -- Binary primitive handling
      binary :: (ExpH -> ExpH -> Maybe ExpH) -> Sig -> ExpH
      binary f s@(Sig _ t) =
        let [ta, tb, _] = unarrowsT t
        in LaceEH (ES_Some WHNF) [
             MatchH (VarP $ Sig (name "a") ta) $ \[(_, a)] ->
               LaceEH (ES_Some WHNF) [
                 MatchH (VarP $ Sig (name "b") tb) $ \[(_, b)] ->
                   let def = AppEH (ES_Some WHNF) (AppEH (ES_Some WHNF) (VarEH (ES_Some SNF) s) a) b
                   in fromMaybe def (f (elab a) (elab b))
                  ]
              ]

      -- Handle a binary primitive of type a -> b -> c
      bXXX :: (ExpH -> Maybe a)     -- ^ how to get a
              -> (ExpH -> Maybe b)  -- ^ how to get b
              -> (c -> ExpH)        -- ^ how to put c
              -> (a -> b -> c)      -- ^ semantics of primitive
              -> (Sig -> ExpH)      -- ^ the generated primitive
      bXXX de_a de_b mkc f =
        let g a b = do
                a' <- de_a a
                b' <- de_b b
                return (mkc (f a' b'))
        in binary g

      -- Binary primitives of various types...
      -- I - Integer
      -- B - Bool
      -- C - Char
      -- V - Bit
      bIII :: (Integer -> Integer -> Integer) -> (Sig -> ExpH)
      bIII = bXXX de_integerEH de_integerEH integerEH

      bIIB :: (Integer -> Integer -> Bool) -> (Sig -> ExpH)
      bIIB = bXXX de_integerEH  de_integerEH boolEH

      bCCB :: (Char -> Char -> Bool) -> (Sig -> ExpH)
      bCCB = bXXX de_charEH de_charEH boolEH

      bVVB :: (Bit -> Bit -> Bool) -> (Sig -> ExpH)
      bVVB = bXXX de_bitEH de_bitEH boolEH

      bVVV :: (Bit -> Bit -> Bit) -> (Sig -> ExpH)
      bVVV = bXXX de_bitEH de_bitEH bitEH

      bVIV :: (Bit -> Integer -> Bit) -> (Sig -> ExpH)
      bVIV = bXXX de_bitEH de_integerEH bitEH

      -- Extract a Bit from an expression of the form: __prim_frominteger_Bit v
      -- The expression should be elaborated already.
      de_bitEH :: ExpH -> Maybe Bit
      de_bitEH (AppEH _ (VarEH _ (Sig fib (AppT _ (AppT _ (NumT w))))) ve)
        | fib == name "Seri.Bit.__prim_fromInteger_Bit"
        , LitEH (IntegerL v) <- elab ve
        = Just (bv_make (nteval w) v)
      de_bitEH _ = Nothing

      stringEH :: String -> ExpH
      stringEH str = toh [] (stringE str)

      primitives :: HT.HashTable Name (Sig -> ExpH)
      primitives = HT.table $ [
            (name "Prelude.__prim_eq_Integer", bIIB (==)),
            (name "Prelude.__prim_add_Integer", bIII (+)),
            (name "Prelude.__prim_sub_Integer", bIII (-)),
            (name "Prelude.__prim_mul_Integer", bIII (*)),
            (name "Prelude.<", bIIB (<)),
            (name "Prelude.<=", bIIB (<=)),
            (name "Prelude.>", bIIB (>)),
            (name "Prelude.__prim_eq_Char", bCCB (==)),
            (name "Seri.Bit.__prim_eq_Bit", bVVB (==)),
            (name "Seri.Bit.__prim_add_Bit", bVVV (+)),
            (name "Seri.Bit.__prim_sub_Bit", bVVV (-)),
            (name "Seri.Bit.__prim_mul_Bit", bVVV (*)),
            (name "Seri.Bit.__prim_concat_Bit", bVVV bv_concat),
            (name "Seri.Bit.__prim_or_Bit", bVVV (.|.)),
            (name "Seri.Bit.__prim_and_Bit", bVVV (.&.)),
            (name "Prelude.&&", 
                let f a b = do
                      val <- de_boolEH a
                      return (if val then b else falseEH)
                in binary f
                ),
            (name "Prelude.||", 
                let f a b = do
                      val <- de_boolEH a
                      return (if val then trueEH else b)
                in binary f
                ),
            (name "Seri.Bit.__prim_shl_Bit", bVVV bv_shl),
            (name "Seri.Bit.__prim_lshr_Bit", bVVV bv_lshr),
            (name "Prelude.__prim_show_Integer", uIS show),
            (name "Seri.Bit.__prim_show_Bit", uVS show),
            (name "Seri.Bit.__prim_not_Bit", uVV complement),
            (name "Prelude.not", uBB not),

            (name "Prelude.error", 
                let g a = do
                    msg <- deStringE (toe a)
                    return (error $ "Seri.error: " ++ msg)
                in unary g
              ),
            (name "Seri.Bit.__prim_extract_Bit", \s@(Sig _ t) ->
                let f :: Bit -> Integer -> Bit
                    f a j = 
                      let AppT _ (NumT wt) = last $ unarrowsT t
                          i = j + (nteval wt) - 1
                      in bv_extract i j a
                in bVIV f s),
            (name "Prelude.valueof", \s@(Sig n t) ->
              let [NumT nt, it] = unarrowsT t
                  g _ = return $ integerEH (nteval nt)
              in unary g s
               ),
            (name "Prelude.numeric", \(Sig _ (NumT nt)) -> ConEH (Sig (name "#" `nappend` name (show (nteval nt))) (NumT nt))),
            (name "Seri.Bit.__prim_zeroExtend_Bit", \s@(Sig _ t) ->
              let [ta, AppT _ (NumT wt)] = unarrowsT t
                  f :: Bit -> Bit
                  f a = bv_zero_extend (nteval wt - bv_width a) a
              in uVV f s
                 ),
            (name "Seri.Bit.__prim_truncate_Bit", \s@(Sig _ t) ->
              let [ta, AppT _ (NumT wt)] = unarrowsT t
                  f :: Bit -> Bit
                  f a = bv_truncate (nteval wt) a
              in uVV f s
                   )
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

de_integerEH :: ExpH -> Maybe Integer
de_integerEH (LitEH (IntegerL i)) = Just i
de_integerEH _ = Nothing

de_charEH :: ExpH -> Maybe Char
de_charEH (LitEH (CharL c)) = Just c
de_charEH _ = Nothing

trueEH :: ExpH
trueEH = ConEH (Sig (name "True") (ConT (name "Bool")))

falseEH :: ExpH
falseEH = ConEH (Sig (name "False") (ConT (name "Bool")))

-- | Boolean expression
boolEH :: Bool -> ExpH
boolEH True = trueEH
boolEH False = falseEH

de_boolEH :: ExpH -> Maybe Bool
de_boolEH x | x == trueEH = Just True
de_boolEH x | x == falseEH = Just False
de_boolEH _ = Nothing

unappsEH :: ExpH -> [ExpH]
unappsEH (AppEH _ a x) = unappsEH a ++ [x]
unappsEH e = [e]

bitEH :: Bit -> ExpH
bitEH b = AppEH (ES_Some SNF) (VarEH (ES_Some SNF) (Sig (name "Seri.Bit.__prim_fromInteger_Bit") (arrowsT [integerT, bitT (bv_width b)]))) (integerEH $ bv_value b)

