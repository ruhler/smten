
{-# LANGUAGE PatternGuards #-}

module Smten.HaskellF.Compile.Type (
    hsType, hsTopType, hsContext, hsClass
    ) where

import qualified Language.Haskell.TH.Syntax as H
import Data.Functor((<$>))

import Smten.Name
import Smten.Type
import Smten.Dec
import Smten.Ppr
import Smten.HaskellF.Compile.HF
import Smten.HaskellF.Compile.Name

-- Convert a Smten type to a Haskell type.
hsType :: Type -> HF H.Type
hsType = hsType' . canonical

-- Convert a Smten type in canonical form to a Haskell type.
hsType' :: Type -> HF H.Type
hsType' t = do
    retype <- asks hfs_retype
    case t of
      _ | Just n <- lookup t retype -> return $ H.VarT (hsName n)
      (ConT n _)
        | n == arrowN -> return H.ArrowT
        | otherwise -> return $ H.ConT (hsqTyName n)
      (AppT a b) -> do
        a' <- hsType' a
        b' <- hsType' b
        return $ H.AppT a' b'
      (VarT n _) -> return $ H.VarT (hsName n)
      (NumT i) -> return $ hsnt i
      (OpT f a b) -> do
        a' <- hsType' a
        b' <- hsType' b
        let f' = case f of
                    "+" -> H.ConT $ H.mkName "Smten.HaskellF.Lib.Numeric.N__PLUS"
                    "-" -> H.ConT $ H.mkName "Smten.HaskellF.Lib.Numeric.N__MINUS"
                    "*" -> H.ConT $ H.mkName "Smten.HaskellF.Lib.Numeric.N__TIMES"
                    _ -> error $ "hsType' TODO: AppNT " ++ f
        return $ H.AppT (H.AppT f' a') b'
      t -> throw $ "haskellf: unsupported type: " ++ pretty t

-- Return the numeric type corresponding to the given integer.
hsnt :: Integer -> H.Type
hsnt 0 = H.ConT (H.mkName "Smten.HaskellF.Lib.Numeric.N__0")
hsnt n = H.AppT (H.ConT (H.mkName $ "Smten.HaskellF.Lib.Numeric.N__2p" ++ show (n `mod` 2))) (hsnt $ n `div` 2)

hsTopType :: Context -> Type -> HF H.Type
hsTopType ctx t = do
    (nctx, use) <- hsContext t
    t' <- hsType t
    ctx' <- mapM hsClass ctx
    case nctx ++ ctx' of
        [] -> return t'
        ctx'' -> return $ H.ForallT (map (H.PlainTV . hsName) use) ctx'' t'

hsClass :: Class -> HF H.Pred
hsClass (Class nm ts) = do
    ts' <- mapM hsType ts
    return $ H.ClassP (hsqTyName nm) ts'
    
-- Form the context for declarations.
--  t - The type to produce the context for. This is used to identify which
--      variable types to declare.
--  Returns the generated context and list of newly declared type variables.
hsContext :: (VarTs a) => a -> HF ([H.Pred], [Name])
hsContext t = do
  retypes <- map snd <$> asks hfs_retype
  tyvars <- asks hfs_tyvars
  let p = flip notElem tyvars
      vts = filter (p . fst) $ (varTs t ++ [(n, NumK) | n <- retypes])
      tvs = [H.ClassP (nmk "Smten.HaskellF.HaskellF.HaskellF" k) [H.VarT (hsName n)] | (n, k) <- vts]
  return (tvs, map fst vts)


