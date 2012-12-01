
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

-- | HOAS form for Seri Expressions, geared towards high performance
-- elaboration.
module Seri.ExpH.ExpH (
    ExpH(..),
    ) where

import Data.Dynamic

import Seri.Lit
import Seri.Type
import Seri.Sig

data ExpH = LitEH Lit
          | ConEH Sig
          | VarEH Sig
          | PrimEH Sig ([ExpH] -> ExpH) [ExpH]
          | AppEH ExpH ExpH
          | LamEH Sig (ExpH -> ExpH)
          | CaseEH ExpH Sig ExpH ExpH
            -- ^ case e1 of
            --      k -> e2
            --      _ -> e3
            -- Note: if k is a constructor of type (a -> b -> c -> K),
            -- Then e2 should have type: (a -> b -> c -> V),
            -- And  e1 should have type: V
            --  Where V is the type of the case expression.
          | ErrorEH Type String
    deriving(Eq, Typeable, Show)

instance Eq (ExpH -> ExpH) where
    (==) _ _ = False

instance Eq ([ExpH] -> ExpH) where
    (==) _ _ = False

instance Show (ExpH -> ExpH) where
    show _ = "..."

instance Show ([ExpH] -> ExpH) where
    show _ = "..."

