
{-# LANGUAGE PatternGuards #-}

module Smten.Plugin.Type (
    typeCG
    ) where

import GhcPlugins

import Data.Functor

import Smten.Plugin.CG
import Smten.Plugin.Name
import qualified Smten.Plugin.Output.Syntax as S

typeCG :: Type -> CG S.Type
typeCG t = subst t >>= typeCG'

typeCG' :: Type -> CG S.Type
typeCG' t 
 | Just (tycon, args) <- splitTyConApp_maybe t = do
     k <- qnameCG (tyConName tycon)
     args' <- mapM typeCG' args
     return $ S.ConAppT k args'
 | (vs@(_:_), t) <- splitForAllTys t = do
     vs' <- mapM (qnameCG . varName) vs
     t' <- typeCG' t
     return $ S.ForallT vs' t'
 | Just v <- getTyVar_maybe t = S.VarT <$> qnameCG (varName v)
 | Just (a, b) <- splitAppTy_maybe t = do
     a' <- typeCG' a
     b' <- typeCG' b
     return $ S.AppT a' b'
 | otherwise = do
     lift $ errorMsg (text "Unsupported type in typeCG': " <+> (ppr t))
     return (S.ConAppT "???" [])

