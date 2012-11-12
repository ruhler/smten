
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}

module Seri.Elaborate.ExpH (
      Mode(..), EState(..), ExpH(..),
      varEH, conEH, appEH, unappsEH, ifEH,
      transform, query,
      de_varEH,
      de_appv1, de_appv2,

      unitEH, boolEH, trueEH, falseEH, integerEH, bitEH,
      de_charEH,
  ) where

import Data.Monoid

import Seri.Bit
import Seri.Lambda hiding (transform, query)
import Seri.Lambda.Ppr hiding (Mode, (<>))

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

conEH :: Sig -> ExpH
conEH = ConEH

varEH :: Sig -> ExpH
varEH = VarEH

appEH :: ExpH -> [ExpH] -> ExpH
appEH f [] = f
appEH f (x:xs) = appEH (AppEH ES_None f x) xs

ifEH :: ExpH -> ExpH -> ExpH -> ExpH
ifEH p a b = 
  let false = CaseEH ES_None p (Sig (name "False") boolT) b (error "if failed to match")
  in CaseEH ES_None p (Sig (name "True") boolT) a false

unappsEH :: ExpH -> [ExpH]
unappsEH (AppEH _ a x) = unappsEH a ++ [x]
unappsEH e = [e]

-- Perform a generic transformation on an expression.
-- Applies the given function to each subexpression. Any matching
-- subexpression is replaced with the returned value, otherwise it continues
-- to recurse.
transform :: (ExpH -> Maybe ExpH) -> ExpH -> ExpH
transform g e | Just v <- g e = v
transform g e 
  = case e of
       LitEH {} -> e
       ConEH {} -> e
       VarEH {} -> e 
       AppEH _ f x -> AppEH ES_None (transform g f) (transform g x)
       LamEH _ s f -> LamEH ES_None s $ \x -> transform g (f x)
       CaseEH _ x k y d -> CaseEH ES_None (transform g x) k (transform g y) (transform g d)

query :: Monoid m => (ExpH -> m) -> ExpH -> m
query g e 
 = g e <> case e of
             AppEH _ f x -> query g f <> query g x
             LamEH _ s f -> query g (f (varEH s))
             CaseEH _ x _ y d -> query g x <> query g y <> query g d
             _ -> mempty
    
     
de_varEH :: ExpH -> Maybe Sig
de_varEH (VarEH s) = Just s
de_varEH _ = Nothing

trueEH :: ExpH
trueEH = ConEH (Sig (name "True") (ConT (name "Bool")))

falseEH :: ExpH
falseEH = ConEH (Sig (name "False") (ConT (name "Bool")))

-- | Boolean expression
boolEH :: Bool -> ExpH
boolEH True = trueEH
boolEH False = falseEH

bitEH :: Bit -> ExpH
bitEH b = AppEH (ES_Some SNF) (VarEH (Sig (name "Seri.Bit.__prim_fromInteger_Bit") (arrowsT [integerT, bitT (bv_width b)]))) (integerEH $ bv_value b)

integerEH :: Integer -> ExpH
integerEH = LitEH . IntegerL 

unitEH :: ExpH
unitEH = conEH (Sig (name "()") unitT)

-- Match an application of the variable with given name to a single argument.
-- Returns the argument.
de_appv1 :: Name -> ExpH -> Maybe ExpH
de_appv1 n e 
    | [v, x] <- unappsEH e
    , Just (Sig nm _) <- de_varEH v
    , n == nm
    = Just x
de_appv1 _ _ = Nothing

de_appv2 :: Name -> ExpH -> Maybe (ExpH, ExpH)
de_appv2 n e
    | [v, x, y] <- unappsEH e
    , Just (Sig nm _) <- de_varEH v
    , n == nm
    = Just (x, y)
de_appv2 _ _ = Nothing

de_charEH :: ExpH -> Maybe Char
de_charEH (LitEH (CharL c)) = Just c
de_charEH _ = Nothing

