
{-# LANGUAGE PatternGuards #-}

module Seri.Type.Ppr () where

import Seri.Ppr
import Seri.Type.Type
import Seri.Type.Sugar

instance Ppr Type where
    ppr t | Just v <- de_listT t = text "[" <> ppr v <> text "]"
    ppr t = error $ "TODO: Type.ppr " ++ show t

