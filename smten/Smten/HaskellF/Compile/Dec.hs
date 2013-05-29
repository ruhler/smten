
{-# LANGUAGE PatternGuards #-}

module Smten.HaskellF.Compile.Dec (hsDec)
    where

import qualified Language.Haskell.TH.Syntax as H

import Smten.Type
import Smten.Name
import Smten.Dec
import Smten.HaskellF.Compile.HF
import Smten.HaskellF.Compile.Name
import Smten.HaskellF.Compile.Type
import Smten.HaskellF.Compile.Exp
import Smten.HaskellF.Compile.Data


hsMethod :: Class -> Method -> HF [H.Dec]
hsMethod cls (Method n e) = do
    env <- asks hfs_env
    mt <- lookupMethodType env n cls
    mctx <- lookupMethodContext env n cls
    hsTopExp (TopExp (TopSig n mctx mt) e)

mkretype :: Type -> [(Type, Name)]
mkretype t
   | OpT {} <- t = [(t, retypenm t)]
   | AppT a b <- t = mkretype a ++ mkretype b
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
   | otherwise = error "unexpected type in HaskellF.Compile.retypenm"


hsTopExp :: TopExp -> HF [H.Dec]
hsTopExp (TopExp (TopSig n ctx t) e) = 
 local (\s -> s { hfs_retype = mkretype (canonical t)}) $ do
     t' <- hsTopType ctx t
     e' <- hsExp e
     let hsn = hsName n
     let sig = H.SigD hsn t'
     let val = H.FunD hsn [H.Clause [] (H.NormalB e') []]
     return [sig, val]
    
hsDec :: Dec -> HF [H.Dec]
hsDec (ValD _ e) = hsTopExp e

hsDec (DataD _ n _ _) | n == arrowN = return []
hsDec (DataD _ n tyvars constrs) = hsData n tyvars constrs

hsDec (ClassD _ ctx n vars exps@(TopExp (TopSig _ _ t) _:_)) = do
    -- Kind inference doesn't currently update the kinds of the vars in the
    -- ClassD declaration, so we look at the vars in one of the method
    -- declarations to figure out the right kind.
    -- TODO: Kind inference should update the vars in the ClassD declaration,
    -- and we should use those here.
    let tvs = filter (flip elem (map tyVarName vars) . fst) (varTs t) 
    (nctx, tyvars) <- hsContext tvs
    local (\s -> s { hfs_tyvars = tyvars}) $ do
        ctx' <- mapM hsClass ctx
        exps' <- mapM hsTopExp exps
        return $ [H.ClassD (nctx ++ ctx') (hsName n) (map (H.PlainTV . hsName) (map tyVarName vars)) [] (concat exps')]

hsDec (InstD _ ctx cls@(Class n ts) ms) = do
    (nctx, tyvars) <- hsContext (appsT (conT n) ts)
    local (\s -> s { hfs_tyvars = tyvars }) $ do
        ctx' <- mapM hsClass ctx
        ms' <- mapM (hsMethod cls) ms
        ts' <- mapM hsTypeBare ts
        let t = foldl H.AppT (H.ConT (hsqTyName n)) ts'
        return [H.InstanceD (nctx ++ ctx') t (concat ms')] 

hsDec (PrimD _ hsnm s@(TopSig n ctx t)) = do
 local (\s -> s { hfs_retype = mkretype (canonical t)}) $ do
     t' <- hsTopType ctx t
     let e = H.AppE (H.VarE (H.mkName "Smten.HaskellF.HaskellF.primHF"))
                    (H.VarE (H.mkName hsnm))
         hsn = hsName n
         sig = H.SigD hsn t'
         val = H.FunD hsn [H.Clause [] (H.NormalB e) []]
     return [sig, val]
    

