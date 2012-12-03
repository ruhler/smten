
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
          | ErrorEH Type String -- ^ type is type of expression.
    deriving(Typeable, Show)

instance Eq ExpH where
    (==) (LitEH a) (LitEH b) = a == b
    (==) (ConEH a) (ConEH b) = a == b
    (==) (VarEH a) (VarEH b) = a == b
    (==) (PrimEH a _ as) (PrimEH b _ bs) = (a == b) && (as == bs)
    (==) (AppEH af ax) (AppEH bf bx) = af == bf && ax == bx
    (==) (CaseEH ax ak ay an) (CaseEH bx bk by bn)
        = and [ ax == bx, ak == bk, ay == by, an == bn]
    (==) _ _ = False
        

instance Show (ExpH -> ExpH) where
    show _ = "..."

instance Show ([ExpH] -> ExpH) where
    show _ = "..."

