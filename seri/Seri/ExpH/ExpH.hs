
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

-- | HOAS form for Seri Expressions, geared towards high performance
-- elaboration.
module Seri.ExpH.ExpH (
    EState(..), ExpH(..),
    ) where

import Data.Dynamic

import Seri.Lit
import Seri.Sig

data EState = ES_None | ES_Done
    deriving (Eq, Show)

data ExpH = LitEH Lit
          | ConEH Sig
          | VarEH Sig
          | PrimEH Sig ([ExpH] -> ExpH) [ExpH]
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
    deriving(Eq, Typeable, Show)

instance Eq (ExpH -> ExpH) where
    (==) _ _ = False

instance Eq ([ExpH] -> ExpH) where
    (==) _ _ = False

instance Show (ExpH -> ExpH) where
    show _ = "..."

instance Show ([ExpH] -> ExpH) where
    show _ = "..."

