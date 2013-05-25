
{-# LANGUAGE PatternGuards #-}

module Smten.HaskellF.Compile.Type (
    hsType, hsTypeBare, hsTopFunctionType, hsTopType, hsContext, hsClass
    ) where

import qualified Language.Haskell.TH.Syntax as H
import Data.Functor((<$>))

import Smten.Name
import Smten.Type
import Smten.Dec
import Smten.Ppr
import Smten.HaskellF.Compile.HF
import Smten.HaskellF.Compile.Name

hsTypeBare :: Type -> HF H.Type
hsTypeBare = hsType' . canonical

-- Convert a Smten type to a Haskell type.
hsType :: Type -> HF H.Type
hsType t = do
    t' <- hsTypeBare t
    return $ H.AppT (H.ConT (H.mkName "Smten.HaskellF.HaskellF.ExpHF")) t'

-- Convert a Smten type in canonical form to a Haskell type.
hsType' :: Type -> HF H.Type
hsType' t = do
    retype <- asks hfs_retype
    case t of
      _ | Just n <- lookup t retype -> return $ H.VarT (hsName n)
      (ConT n _)
        | n == arrowN -> return $ H.ConT (H.mkName "Smten.HaskellF.HaskellF.T__Function")
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
                    "+" -> H.ConT $ H.mkName "Smten.HaskellF.Numeric.N__PLUS"
                    "-" -> H.ConT $ H.mkName "Smten.HaskellF.Numeric.N__MINUS"
                    "*" -> H.ConT $ H.mkName "Smten.HaskellF.Numeric.N__TIMES"
                    _ -> error $ "hsType' TODO: AppNT " ++ f
        return $ H.AppT (H.AppT f' a') b'
      t -> throw $ "haskellf: unsupported type: " ++ pretty t

-- Return the numeric type corresponding to the given integer.
hsnt :: Integer -> H.Type
hsnt 0 = H.ConT (H.mkName "Smten.HaskellF.Numeric.N__0")
hsnt n = H.AppT (H.ConT (H.mkName $ "Smten.HaskellF.Numeric.N__2p" ++ show (n `mod` 2))) (hsnt $ n `div` 2)

-- Given a list of Smten types [a, b, ..., x]
-- Produce a haskell type corresponding to a haskell function of the form:
--  ExpHF a -> ExpHF b -> ... -> ExpHF x
hsTopFunctionType :: Context -> [Type] -> HF H.Type
hsTopFunctionType ctx ts = do
    (nctx, use) <- hsContext ts
    ts' <- mapM hsType ts
    ctx' <- mapM hsClass ctx
    let arrT :: H.Type -> H.Type -> H.Type
        arrT a b = H.AppT (H.AppT H.ArrowT a) b
    
        t = foldr1 arrT ts'
    case nctx ++ ctx' of
        [] -> return t
        ctx'' -> return $ H.ForallT (map (H.PlainTV . hsName) use) ctx'' t

hsTopType :: Context -> Type -> HF H.Type
hsTopType ctx t = hsTopFunctionType ctx [t]

hsClass :: Class -> HF H.Pred
hsClass (Class nm ts) = do
    ts' <- mapM hsTypeBare ts
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
      tvs = [H.ClassP (nmk "Smten.Type.SmtenT" k) [H.VarT (hsName n)] | (n, k) <- vts]
  return (tvs, map fst vts)


