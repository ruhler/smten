
{-# LANGUAGE PatternGuards #-}

module Smten.Plugin.Type (
    typeCG, topTypeCG, tyvarCG,
    isStarKind, ctxCG,
    ) where

import Kind
import GhcPlugins

import Data.Functor

import Smten.Plugin.CG
import Smten.Plugin.Name
import qualified Smten.Plugin.Output.Syntax as S

-- Return the type without SmtenHSN added to the context.
typeCG :: Type -> CG S.Type
typeCG t = subst t >>= typeCG' False

-- Return the type with SmtenHSN added to context as needed.
topTypeCG :: Type -> CG S.Type
topTypeCG t = subst t >>= typeCG' True

typeCG' :: Bool -> Type -> CG S.Type
typeCG' mkctx t 
 | Just (tycon, args) <- splitTyConApp_maybe t = do
     k <- qtynameCG (tyConName tycon)
     args' <- mapM (typeCG' mkctx) args
     return $ S.ConAppT k args'
 | (vs@(_:_), t) <- splitForAllTys t = do
     vs' <- mapM (qnameCG . varName) vs
     t' <- (typeCG' mkctx) t
     ctx <- concat <$> mapM ctxCG vs
     let usectx = if mkctx then ctx else []
     return $ S.ForallT vs' usectx t'
 | Just v <- getTyVar_maybe t = tyvarCG v
 | Just (a, b) <- splitAppTy_maybe t = do
     a' <- (typeCG' mkctx) a
     b' <- (typeCG' mkctx) b
     return $ S.AppT a' b'
 | Just v <- isNumLitTy t = return $ S.NumT v
 | otherwise = do
     lift $ fatalErrorMsg (text "Unsupported type in typeCG': " <+> (ppr t))
     return (S.ConAppT "???" [])

tyvarCG :: TyVar -> CG S.Type
tyvarCG v = S.VarT <$> qnameCG (varName v)

ctxCG :: TyVar -> CG [S.Class]
ctxCG v
 | tyVarKind v `eqKind` typeNatKind = return []
 | otherwise = do
  tyv <- tyvarCG v
  addimport "Smten.Runtime.SmtenHS"
  return [S.ConAppT ("Smten.Runtime.SmtenHS.SmtenHS" ++ show (knum (tyVarKind v))) [tyv]]

knum :: Kind -> Int
knum k = length (fst (splitKindFunTys k))

isStarKind :: Kind -> Bool
isStarKind = isLiftedTypeKind

subst :: Type -> CG Type
subst t = do
    (tyvs, ts) <- unzip <$> gets cgs_types
    return $ substTyWith tyvs ts t

