
{-# LANGUAGE FlexibleInstances #-}

-- | HOAS form for Seri Expressions, geared towards high performance
-- elaboration.
module Seri.ExpH.ExpH (
    Mode(..), EState(..), ExpH(..),
    ) where

import Seri.Lit
import Seri.Sig

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

instance Eq (ExpH -> ExpH) where
    (==) _ _ = False
