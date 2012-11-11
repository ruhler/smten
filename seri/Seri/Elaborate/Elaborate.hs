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
import Data.List(genericLength)
import Data.Maybe(fromMaybe)
import Data.Monoid

import Seri.Bit
import Seri.Failable
import qualified Seri.HashTable as HT
import Seri.Lambda
import Seri.Lambda.Ppr hiding (Mode)

import Seri.Elaborate.FreshPretty

data Mode = WHNF -- ^ elaborate to weak head normal form.
          | SNF  -- ^ elaborate to smt normal form.
    deriving (Show, Eq, Ord)

data EState = ES_None | ES_Some Mode
    deriving (Show, Eq)

data ExpH = LitEH Lit
          | ConEH Sig
          | VarEH Sig
          | AppEH EState ExpH ExpH
          | LamEH EState Sig (ExpH -> ExpH)
          | CaseEH EState ExpH Sig ExpH ExpH
            -- ^ case e1 of
            --      k -> e2
            --      _ -> e3
            -- Note: if k is a constructor of type (a -> b -> c -> K),
            -- Then e2 should have type: (a -> b -> c -> V),
            -- And  e1 should have type: V
            --  Where V is the type of the case expression.
    deriving(Eq)

instance Ppr ExpH where
    ppr (LitEH l) = ppr l
    ppr (ConEH s) = ppr s
    ppr (VarEH s) = ppr s
    ppr (AppEH _ f x) = parens (ppr f) <+> parens (ppr x)
    ppr (LamEH _ s f) = text "\\" <+> ppr s <+> text "-> ..."
    ppr (CaseEH _ e1 p e2 e3)
        = text "case" <+> parens (ppr e1) <+> text "of" <+> text "{"
            $+$ nest tabwidth (vcat [
                    ppr p <+> text "->" <+> ppr e2,
                    text "_" <+> text "->" <+> ppr e3
                  ]) $+$ text "}"
    
instance Eq (ExpH -> ExpH) where
    (==) _ _ = False

instance Typeof ExpH where
    typeof (LitEH l) = typeof l
    typeof (ConEH s) = typeof s
    typeof (VarEH s) = typeof s
    typeof (AppEH _ f x) =
        let fts = unarrowsT (typeof f)
        in case (drop 1 fts) of
              [] -> UnknownT
              ts -> arrowsT ts
    typeof (LamEH _ v f) = arrowsT [typeof v, typeof (f (VarEH v))]
    typeof (CaseEH _ _ _ _ e) = typeof e


-- Weak head normal form elaboration
elabwhnf :: Env -> Exp -> Exp
elabwhnf = elaborate WHNF

-- | Elaborate an expression under the given mode.
elaborate :: Mode  -- ^ Elaboration mode
          -> Env   -- ^ context under which to evaluate the expression
          -> Exp   -- ^ expression to evaluate
          -> Exp   -- ^ elaborated expression
elaborate mode env exp =
  let -- elaborate the given expression
      elab :: ExpH -> ExpH
      elab e =
        case e of
          LitEH l -> e
          ConEH s -> e
          VarEH (Sig n t) | Just f <- HT.lookup n nprimitives -> f t
          VarEH s@(Sig n ct) ->
            case (attemptM $ lookupVar env s) of
                Just (pt, ve) -> elab $ toh [] $ assignexp (assignments pt ct) ve
                Nothing -> VarEH s
          AppEH (ES_Some m) _ _ | mode <= m -> e
          AppEH _ f arg -> 
             case (elab f) of
               VarEH (Sig n t)
                 | Just f <- HT.lookup n uprimitives
                 , Just v <- f t (elab arg) -> v
               AppEH _ (VarEH (Sig n t)) x
                 | Just f <- HT.lookup n bprimitives
                 , Just v <- f t (elab x) (elab arg) -> v
               CaseEH _ a k y n | mode == SNF ->
                 let -- Perform argument pushing.
                     -- (case a of
                     --     k -> y
                     --     _ -> n) arg
                     -- Where y = \v1 -> \v2 -> ... -> yv
                     -- Translates to:
                     --     case a of
                     --         k -> \v1 -> \v2 -> ... -> yv arg
                     --         _ -> n arg
                     ybody = \yv -> AppEH ES_None yv arg
                     yify :: Integer -> (ExpH -> ExpH) -> ExpH -> ExpH
                     yify 0 f x = f x
                     yify n f (LamEH _ s b) = LamEH ES_None s $ \x ->
                         (yify (n-1) f (b x))
                     yify n f x = error $ "yify got: " ++ pretty x

                     kargs = genericLength (unarrowsT (typeof k)) - 1

                     y' = yify kargs ybody y
                     n' = AppEH ES_None n arg
                 in elab $ CaseEH ES_None a k y' n'
               LamEH _ _ b -> elab $ b arg
               f' -> AppEH (ES_Some mode) f' (if mode == SNF then elab arg else arg)
          LamEH (ES_Some m) _ _ | mode <= m -> e
          LamEH {} | mode == WHNF -> e
          LamEH _ v f -> LamEH (ES_Some mode) v (\x -> elab (f x))
          CaseEH (ES_Some m) _ _ _ _ | mode <= m -> e
          CaseEH _ arg k@(Sig nk _) yes no
            | (ConEH (Sig s _):vs) <- unappsEH (elab arg) ->
              if s == nk
                then elab $ appEH yes vs
                else elab no
          CaseEH _ arg k1 y1 n1
            | mode == SNF
            , CaseEH _ x2 k2 y2 n2 <- elab arg ->
                let -- Decasify:
                    --  case (case x2 of k2 -> y2 ; _ -> n2) of
                    --      k1 -> y1;
                    --      _ -> n1;
                    --
                    --  Where: y2 = \v1 -> \v2 -> ... -> y2v
                    --
                    -- Turns into:
                    --  case x2 of
                    --     k2 -> \v1 -> \v2  -> ... ->
                    --                case y2v of
                    --                    k1 -> y1;
                    --                    _ -> n1;
                    --     _ -> case n2 of
                    --            k1 -> y1;
                    --            _ -> n1;
                    -- TODO: use lets to maintain sharing of y1 and n1.
                    y2body = \y -> CaseEH ES_None y k1 y1 n1
                    y2ify :: Integer -> (ExpH -> ExpH) -> ExpH -> ExpH
                    y2ify 0 f x = f x
                    y2ify n f (LamEH _ s b) = LamEH ES_None s $ \x ->
                        (y2ify (n-1) f (b x))
                    y2ify n f x = error $ "y2ify got: " ++ pretty x

                    k2args = genericLength (unarrowsT (typeof k2)) - 1

                    y2' = y2ify k2args y2body y2
                    n2' = CaseEH ES_None n2 k1 y1 n1
                in elab $ CaseEH ES_None x2 k2 y2' n2'
          CaseEH _ arg@(VarEH (Sig nm t)) k yes no
              | mode == SNF && t == boolT ->
                let Just v = de_boolEH (ConEH k)
                in CaseEH (ES_Some mode) (elab arg) k (elab (concretize nm v yes)) (elab (concretize nm (not v) no))
          CaseEH _ arg k yes no | mode == SNF ->
            CaseEH (ES_Some mode) (elab arg) k (elab yes) (elab no)
          CaseEH _ arg k yes no -> CaseEH (ES_Some mode) arg k yes no
        
      toe :: ExpH -> Exp
      toe e = runFresh (toeM e) (free' exp)

      -- Extract a Bit from an expression of the form: __prim_frominteger_Bit v
      -- The expression should be elaborated already.
      de_bitEH :: ExpH -> Maybe Bit
      de_bitEH (AppEH _ (VarEH (Sig fib (AppT _ (AppT _ (NumT w))))) ve)
        | fib == name "Seri.Bit.__prim_fromInteger_Bit"
        , LitEH (IntegerL v) <- elab ve
        = Just (bv_make (nteval w) v)
      de_bitEH _ = Nothing

      -- nullary primitives
      nprimitives :: HT.HashTable Name (Type -> ExpH)
      nprimitives = HT.table $ [
            (name "Prelude.numeric", \(NumT nt) ->
                ConEH (Sig (name "#" `nappend` name (show (nteval nt))) (NumT nt)))
              ]

      -- unary primitives
      uprimitives :: HT.HashTable Name (Type -> ExpH -> Maybe ExpH)
      uprimitives =
        let uXX :: (ExpH -> Maybe a) -> (b -> ExpH) -> (a -> b)
                   -> Type -> ExpH -> Maybe ExpH
            uXX de_a mkb f _ a = do 
                a' <- de_a a
                return (mkb (f a'))

            uBB = uXX de_boolEH boolEH
            uVV = uXX de_bitEH bitEH
            uIS = uXX de_integerEH stringEH
            uVS = uXX de_bitEH stringEH
        in HT.table $ [
             (name "Prelude.not", uBB not),
             (name "Seri.Bit.__prim_not_Bit", uVV complement),
             (name "Prelude.__prim_show_Integer", uIS show),
             (name "Seri.Bit.__prim_show_Bit", uVS show),
             (name "Prelude.&&", \_ a -> do
                 a' <- de_boolEH a
                 return $ LamEH ES_None (Sig (name "b") boolT)
                    (if a' then id else const falseEH)
                ),
             (name "Prelude.||", \_ a -> do
                 a' <- de_boolEH a
                 return $ LamEH ES_None (Sig (name "b") boolT)
                    (if a' then const trueEH else id)
                ),
--             (name "Prelude.error", \_ a -> do
--                 a <- deStringE (toe a)
--                 return (error $ "Seri.error: " ++ msg)
--                ),
             (name "Prelude.valueof", \t _ -> 
               let [NumT nt, it] = unarrowsT t
               in return $ integerEH (nteval nt)
                ),
             (name "Seri.Bit.__prim_zeroExtend_Bit", \t a -> do
               let [ta, AppT _ (NumT wt)] = unarrowsT t
               a' <- de_bitEH a
               return . bitEH $ bv_zero_extend (nteval wt - bv_width a') a'
                  ),
             (name "Seri.Bit.__prim_truncate_Bit", \t a -> do
               let [ta, AppT _ (NumT wt)] = unarrowsT t
               a' <- de_bitEH a
               return . bitEH $ bv_truncate (nteval wt) a'
                   )
               ]

      -- binary primitives
      bprimitives :: HT.HashTable Name (Type -> ExpH -> ExpH -> Maybe ExpH)
      bprimitives =
        let bXXX :: (ExpH -> Maybe a) -> (ExpH -> Maybe b) -> (c -> ExpH)
                    -> (a -> b -> c)
                    -> Type -> ExpH -> ExpH -> Maybe ExpH
            bXXX de_a de_b mkc f _ a b = do
                a' <- de_a a
                b' <- de_b b
                return (mkc (f a' b'))

            bIIB = bXXX de_integerEH de_integerEH boolEH
            bIII = bXXX de_integerEH de_integerEH integerEH
            bCCB = bXXX de_charEH de_charEH boolEH
            bVVB = bXXX de_bitEH de_bitEH boolEH
            bVVV = bXXX de_bitEH de_bitEH bitEH

            bSXXX :: (ExpH -> Maybe a) -> (ExpH -> Maybe b) -> (c -> ExpH)
                    -> (Type -> a -> b -> c)
                    -> Type -> ExpH -> ExpH -> Maybe ExpH
            bSXXX de_a de_b mkc f t a b = do
                a' <- de_a a
                b' <- de_b b
                return (mkc (f t a' b'))

            bSVIV = bSXXX de_bitEH de_integerEH bitEH
        in HT.table $ [
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
             (name "Seri.Bit.__prim_shl_Bit", bVVV bv_shl),
             (name "Seri.Bit.__prim_lshr_Bit", bVVV bv_lshr),
             (name "Seri.Bit.__prim_extract_Bit",
                let f :: Type -> Bit -> Integer -> Bit
                    f t a j =
                      let AppT _ (NumT wt) = last $ unarrowsT t
                          i = j + (nteval wt) - 1
                      in bv_extract i j a
                in bSVIV f)
                ]

      exph = toh [] exp
      elabed = elab exph
      done = toe elabed
  in --trace ("elab " ++ show mode ++ ": " ++ pretty exp) $
     --trace ("To: " ++ pretty done) $
     done


assignexp :: [(Name, Type)] -> Exp -> Exp
assignexp = assign
        
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

appEH :: ExpH -> [ExpH] -> ExpH
appEH f [] = f
appEH f (x:xs) = appEH (AppEH ES_None f x) xs

bitEH :: Bit -> ExpH
bitEH b = AppEH (ES_Some SNF) (VarEH (Sig (name "Seri.Bit.__prim_fromInteger_Bit") (arrowsT [integerT, bitT (bv_width b)]))) (integerEH $ bv_value b)

ifEH :: ExpH -> ExpH -> ExpH -> ExpH
ifEH p a b = 
  let false = CaseEH ES_None p (Sig (name "False") boolT) b (error "if failed to match")
  in CaseEH ES_None p (Sig (name "True") boolT) a false

-- Translate back to the normal Exp representation
toeM :: ExpH -> Fresh Exp
toeM (LitEH l) = return (LitE l)
toeM (ConEH s) = return (ConE s)
toeM (VarEH s) = return (VarE s)
toeM e@(AppEH {}) = do
  (f:args) <- mapM toeM (unappsEH e)
  return (AppE f args)
toeM (LamEH _ s f) = do
  s' <- fresh s
  b <- toeM (f (VarEH s'))
  return (lamE $ Match [VarP s'] b)
toeM (CaseEH _ arg (Sig n t) yes no) = do
  arg' <- toeM arg
  let tys = unarrowsT t
  vars <- mapM (fresh . Sig (name "_x")) (init tys)
  yes' <- toeM (appEH yes (map VarEH vars))
  no' <- toeM no
  let pt = typeof arg'
  return $ caseE arg' [Match [ConP pt n (map VarP vars)] yes',
                       Match [WildP pt] no']

-- Replace all occurences of the boolean variable with given name to the value
-- True or False in the given expression.
concretize :: Name -> Bool -> ExpH -> ExpH
concretize n v e
 = case e of
    LitEH {} -> e
    ConEH {} -> e
    VarEH (Sig nm _) | n == nm -> boolEH v
    VarEH {} -> e
    AppEH _ f x -> AppEH ES_None (concretize n v f) (concretize n v x)
    LamEH _ s f -> LamEH ES_None s $ \x -> concretize n v (f x)
    CaseEH _ x k y d -> CaseEH ES_None (concretize n v x) k (concretize n v y) (concretize n v d)

-- translate to our HOAS expression representation
toh :: [(Sig, ExpH)] -> Exp -> ExpH
toh _ (LitE l) = LitEH l
toh _ (ConE s) = ConEH s
toh m (VarE s) | Just v <- lookup s m = v
toh m (VarE s) = VarEH s
toh m (AppE f xs) =
  let appeh :: ExpH -> Exp -> ExpH
      appeh f x = AppEH ES_None f (toh m x)
  in foldl appeh (toh m f) xs
toh m e | Just (Match [VarP s] b) <- deLamE e =   
  LamEH ES_None s $ \x -> toh ((s, x):m) b
toh m e@(LaceE ms@(Match [_] _ : _)) =
  let deSugarLace :: [(Sig, ExpH)] -> [Match] -> ExpH
      deSugarLace vars ms@(Match [p] b : _) =
        let tpat = typeof p
            tbody = typeof b
            terr = arrowsT [stringT, tbody]
            errv = VarEH (Sig (name "Prelude.error") terr)
            err = AppEH ES_None errv (stringEH "Case no match")

            depat :: [(Sig, ExpH)] -- ^ Variables in scope
                  -> ExpH -- ^ Argument to the case expression
                  -> Pat  -- ^ Pattern to match against
                  -> ([(Sig, ExpH)] -> ExpH) -- ^ Body if match succeeds   
                  -> ExpH -- ^ Default value on failure
                  -> ExpH
            depat vars arg (WildP {}) b _ = b vars
            depat vars arg (VarP s) b _ =
              let lam = LamEH ES_None s $ \x -> b ((s, x):vars)
              in AppEH ES_None lam arg
            depat vars arg (LitP l) b def =
              let lt = typeof l
                  eqt = arrowsT [lt, lt, boolT]
                  p = appEH (VarEH (Sig (name "Prelude.==") eqt)) [LitEH l, arg]
              in ifEH p (b vars) def
            depat vars arg (ConP t n ps) b def =
              let k = Sig n (arrowsT ((map typeof ps) ++ [t]))
                  mkmatched :: [(Sig, ExpH)] -> [(Pat, ExpH)] -> ([(Sig, ExpH)] -> ExpH) -> ExpH -> ExpH
                  mkmatched vars [] b _ = b vars
                  mkmatched vars ((p, x):ps) b def =
                    let body = \vs -> mkmatched vs ps b def
                    in depat vars x p body def
  
                  mklams :: [(Sig, ExpH)] -> [Pat] -> [(Pat, ExpH)] -> ([(Sig, ExpH)] -> ExpH) -> ExpH -> ExpH
                  mklams vars [] ps body def = mkmatched vars ps body def
                  mklams vars (p:ps) ps' body def =
                    LamEH ES_None (Sig (name ("_cb")) (typeof p)) $ \x ->
                      mklams vars ps ((p, x):ps') body def
              in CaseEH ES_None arg k (mklams vars ps [] b def) def
            
            -- perform core desugaring of case statements.
            desugar :: ExpH -- ^ Argument to the case expression
                    -> [Match] -- ^ Set of matches
                    -> ExpH -- ^ The default value if no match
                    -> ExpH -- ^ The desugared expression
            desugar arg [Match [p] b] def = depat vars arg p (flip toh b) def
            desugar arg (m:ms) def = desugar arg [m] (desugar arg ms def)

        in LamEH (ES_Some WHNF) (Sig (name "_ca") tpat) $ \ca ->
              desugar ca ms err
  in deSugarLace m ms
toh m (LaceE ms) = toh m (sLaceE ms)

stringEH :: String -> ExpH
stringEH str = toh [] (stringE str)


