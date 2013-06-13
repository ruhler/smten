
{-# LANGUAGE PatternGuards #-}

module Smten.CodeGen.Type(
    typeCG, classCG, topSigCG, contextCG, knum, retyped,
    primtypeCG,
   ) where

import qualified Language.Haskell.TH.Syntax as H

import Data.Functor((<$>))

import Smten.Name
import Smten.Type
import Smten.Dec
import Smten.CodeGen.CG
import Smten.CodeGen.Name

typeCG :: Type -> CG H.Type
typeCG = typeCG' . canonical

-- Code generation for a canonicalized type.
typeCG' :: Type -> CG H.Type
typeCG' t = do
  retype <- asks cg_retype
  case t of
    _ | Just n <- lookup t retype -> return $ H.VarT (nameCG n)
    ConT n _
     | n == arrowN -> return H.ArrowT
     | otherwise -> return $ H.ConT (qtynameCG n)

    AppT a b -> do
       a' <- typeCG' a
       b' <- typeCG' b
       return $ H.AppT a' b'

    VarT n _ -> return $ H.VarT (nameCG n)

    NumT i -> return $ H.AppT (H.ConT (H.mkName "Smten.NumT"))
                              (H.LitT (H.NumTyLit i))
    OpT op a b -> do
        a' <- typeCG' a
        b' <- typeCG' b
        return $ foldl1 H.AppT [H.ConT (H.mkName ("(Smten.:" ++ op ++ ":)")), a', b']
                    
    UnknownT -> error $ "typeCG: unknown type encountered"

classCG :: Class -> CG H.Pred
classCG (Class n tys) = do
    tys' <- mapM typeCG tys
    return $ H.ClassP (qtynameCG n) tys'

topSigCG :: TopSig -> CG H.Dec
topSigCG (TopSig nm ctx t) = do
    t' <- typeCG t
    (tyvs, ctx') <- contextCG t ctx
    return $ H.SigD (nameCG nm) (H.ForallT tyvs ctx' t')

contextCG :: (VarTs a) => a -> Context -> CG ([H.TyVarBndr], [H.Pred])
contextCG x ctx = do 
    retypes <- map snd <$> asks cg_retype
    tyvnmsbound <- asks cg_tyvars
    ctx' <- mapM classCG ctx
    let tyvnmsall = varTs x ++ [(n, NumK) | n <- retypes]
        tyvnmslocal = filter (flip notElem tyvnmsbound . fst) tyvnmsall
        tyvs = map (H.PlainTV . nameCG . fst) tyvnmslocal
        shsctx = concatMap smtenhsCG tyvnmslocal
    return (tyvs, shsctx ++ ctx')

knum :: Kind -> Integer
knum (ArrowK a b) = 1 + knum b
knum _ = 0

smtenhsCG :: (Name, Kind) -> [H.Pred]
smtenhsCG (n, k) =
  let smhs = H.ClassP (H.mkName $ "Smten.SmtenHS" ++ show (knum k)) [H.VarT $ nameCG n]
      num = H.ClassP (H.mkName $ "Smten.Numeric") [H.VarT $ nameCG n]
  in case k of
        NumK -> [smhs, num]
        _ -> [smhs]

retyped :: Type -> CG a -> CG a
retyped t = local (\cg -> cg { cg_retype = retypeCG (canonical t) })

retypeCG :: Type -> [(Type, Name)]
retypeCG t
   | OpT {} <- t = [(t, retypenm t)]
   | AppT a b <- t = retypeCG a ++ retypeCG b
   | otherwise = []

retypenm :: Type -> Name
retypenm t
   | VarT n _ <- t = n
   | OpT o a b <- t =
       let an = retypenm a 
           bn = retypenm b
           opn = case o of
                   "+" -> "_plus_"
                   "-" -> "_minus_"
                   "*" -> "_times_"
       in name $ unname an ++ opn ++ unname bn
   | NumT i <- t = name ("_" ++ show i)
   | otherwise = error "unexpected type in Smten.CodeGen.Dec.retypenm"

primtypeCG :: String -> Type -> CG H.Type
primtypeCG hsmod = primtypeCG' hsmod . canonical

-- Code generation for a canonicalized primitive type.
-- Notes:
--   * haskell types are assumed to have the same names as smten types
--   * haskell types are assumed to be exported by the same module as the
--     haskell primitive
--   * polymorphic types stay polymorphic
--   * It is the user's burden to ensure there are appropriate instances of
--     Haskelly
primtypeCG' :: String -> Type -> CG H.Type
primtypeCG' hsmod t = do
  retype <- asks cg_retype
  case t of
    _ | Just n <- lookup t retype -> return $ H.VarT (nameCG n)
    ConT n _
     | n == arrowN -> return H.ArrowT
     | n == listN -> return H.ListT
     | n == unitN -> return $ H.ConT (H.mkName "()")
     | otherwise -> return $ H.ConT (H.mkName $ hsmod ++ "." ++ unname (unqualified n))

    AppT a b -> do
       a' <- primtypeCG' hsmod a
       b' <- primtypeCG' hsmod b
       return $ H.AppT a' b'

    VarT n _ -> return $ H.VarT (nameCG n)

    NumT i -> return $ H.AppT (H.ConT (H.mkName "Smten.NumT"))
                              (H.LitT (H.NumTyLit i))
    OpT op a b -> do
        a' <- primtypeCG' hsmod a
        b' <- primtypeCG' hsmod b
        return $ foldl1 H.AppT [H.ConT (H.mkName ("(Smten.:" ++ op ++ ":)")), a', b']
                    
    UnknownT -> error $ "primtypeCG: unknown type encountered"
