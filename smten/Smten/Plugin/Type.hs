
{-# LANGUAGE PatternGuards #-}

module Smten.Plugin.Type (
    typeCG, tyvarCG,
    isStarKind, ctxCG,
    ) where

import Kind
import GhcPlugins

import Data.Functor

import Smten.Plugin.CG
import Smten.Plugin.Name
import qualified Smten.Plugin.Output.Syntax as S

typeCG :: Type -> CG S.Type
typeCG t 
 | Just (tycon, args) <- splitTyConApp_maybe t = do
     k <- qtynameCG (tyConName tycon)
     args' <- mapM typeCG args
     return $ S.ConAppT k args'
 | (vs@(_:_), t) <- splitForAllTys t = do
     vs' <- mapM (qnameCG . varName) vs
     t' <- typeCG t
     ctx <- concat <$> mapM ctxCG vs
     return $ S.ForallT vs' ctx t'
 | Just v <- getTyVar_maybe t = tyvarCG v
 | Just (a, b) <- splitAppTy_maybe t = do
     a' <- typeCG a
     b' <- typeCG b
     return $ S.AppT a' b'
 | Just v <- isNumLitTy t = return $ S.NumT v
 | otherwise = do
     lift $ fatalErrorMsg (text "Unsupported type in typeCG: " <+> (ppr t))
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

