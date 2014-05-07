
{-# LANGUAGE PatternGuards #-}

module Smten.Plugin.Type (
    typeCG, topTypeCG,
    castInnerTypeCG, castOuterTypeCG,
    tyvarCG,
    isStarKind, ctxCG,
    ) where

import Kind
import GhcPlugins

import Control.Monad.State
import Data.Functor

import Smten.Plugin.CG
import Smten.Plugin.Name
import qualified Smten.Plugin.Output.Syntax as S

-- Return the type without SmtenHSN added to the context.
typeCG :: Type -> CG S.Type
typeCG t = evalStateT (typeCG' t) (TyCGState True False [] [])

-- Return the type with SmtenHSN added to context as needed.
topTypeCG :: Type -> CG S.Type
topTypeCG t = evalStateT (typeCG' t) (TyCGState True True [] [])

-- Return a type with no bound type variables.
castInnerTypeCG :: Type -> CG S.Type
castInnerTypeCG t = evalStateT (typeCG' t) (TyCGState False False [] [])

-- Return a type with all bound variables at the top level.
castOuterTypeCG :: Type -> CG S.Type
castOuterTypeCG t = do
  (t', s) <- runStateT (typeCG' t) (TyCGState False False [] [])
  return $ S.ForallT (s_bound s) (s_ctx s) t'

-- Configuration and results for typeCG' function.
data TyCGState = TyCGState {
    -- | mkforall: True to generate forall types.
    -- Note: this is intended to be read only
    s_mkforall :: Bool,

    -- | mkctx - True to generate SmtenHS* contexts for forall types
    -- Note: this is intended to be read only
    s_mkctx :: Bool,

    -- | All bound type variables.
    -- Note: this is intended to be write only
    s_bound :: [S.TyVar],

    -- | SmtenHS* context for all bound type variables.
    -- Note: this is intended to be write only
    s_ctx :: [S.Class]
}

-- typeCG'
typeCG' :: Type -> StateT TyCGState CG S.Type
typeCG' t 
 | Just (tycon, args) <- splitTyConApp_maybe t = do
     k <- lift $ qtynameCG (tyConName tycon)
     args' <- mapM typeCG' args
     return $ S.ConAppT k args'
 | (vs@(_:_), t) <- splitForAllTys t = do
     vs' <- lift $ mapM (qnameCG . varName) vs
     t' <- typeCG' t
     s <- get
     ctx <- lift $ concat <$> mapM ctxCG vs
     let usectx = if s_mkctx s then ctx else []
     put $ s { s_bound = vs' ++ s_bound s, s_ctx = ctx ++ s_ctx s }
     return (if s_mkforall s then S.ForallT vs' usectx t' else t')
 | Just v <- getTyVar_maybe t = lift $ tyvarCG v
 | Just (a, b) <- splitAppTy_maybe t = do
     a' <- typeCG' a
     b' <- typeCG' b
     return $ S.AppT a' b'
 | Just v <- isNumLitTy t = return $ S.NumT v
 | otherwise = do
     lift . lift $ fatalErrorMsg (text "Unsupported type in typeCG': " <+> (ppr t))
     return (S.ConAppT "???" [])

tyvarCG :: TyVar -> CG S.Type
tyvarCG v = S.VarT <$> qnameCG (varName v)

ctxCG :: TyVar -> CG [S.Class]
ctxCG v
 | isLinearKind (tyVarKind v) = do
    tyv <- tyvarCG v
    smtenhs <- usequalified "Smten.Runtime.SmtenHS" "SmtenHS"
    return [S.ConAppT (smtenhs ++ show (knum (tyVarKind v))) [tyv]]
 | otherwise = return []

knum :: Kind -> Int
knum k = length (fst (splitKindFunTys k))

isStarKind :: Kind -> Bool
isStarKind = isLiftedTypeKind

isLinearKind :: Kind -> Bool
isLinearKind k =
  let (args, res) = splitKindFunTys k
  in all isStarKind (args) && isStarKind res

