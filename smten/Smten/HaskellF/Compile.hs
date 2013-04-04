-------------------------------------------------------------------------------
-- Copyright (c) 2012      SRI International, Inc. 
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory under DARPA/AFRL contract (FA8750-10-C-0237)
-- ("CTSRD"), as part of the DARPA CRASH research programme.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
-------------------------------------------------------------------------------
--
-- Authors: 
--   Richard Uhler <ruhler@csail.mit.edu>
-- 
-------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Back end target which translates smten programs into Haskell. Supports the
-- Query monad and SMT queries.
module Smten.HaskellF.Compile (
    haskellf,
    ) where

import Data.Functor((<$>))
import Data.List(genericLength, genericReplicate)

import Control.Monad.Error
import Control.Monad.Reader

import qualified Language.Haskell.TH.PprLib as H
import qualified Language.Haskell.TH as H
import qualified Language.Haskell.TH.Syntax as H

import Smten.Failable
import Smten.Name
import Smten.Sig
import Smten.Type
import Smten.Lit
import Smten.Exp
import Smten.Dec
import Smten.Ppr
import Smten.HaskellF.Compile.HF
import Smten.HaskellF.Compile.Name
import Smten.HaskellF.Compile.Kind
import Smten.HaskellF.Compile.Ppr

hsLit :: Lit -> H.Exp
hsLit l
 | Just i <- de_integerL l = H.LitE (H.IntegerL i)
 | Just c <- de_charL l = H.AppE (H.VarE (H.mkName "S.smtenHF")) (H.LitE (H.CharL c))

hsExp :: Exp -> HF H.Exp

-- String literals:
-- TODO: the template haskell pretty printer doesn't print strings correctly
-- if they contain newlines, thus, we can't print those as string literals.
-- When they fix the template haskell pretty printer, that special case should
-- be removed here.
hsExp e
  | Just str <- de_stringE e
  , '\n' `notElem` str
    = return $ H.AppE (H.VarE (H.mkName "S.smtenHF")) (H.LitE (H.StringL str))

hsExp (LitE _ l) = return (hsLit l)
hsExp (ConE _ (Sig n _)) = return $ H.ConE (hsName n)
hsExp (VarE _ (Sig n t)) = do
    -- Give explicit type signature to make sure there are no type ambiguities
    ht <- hsType t
    return $ H.SigE (H.VarE (hsName n)) ht
hsExp (AppE _ f x) = do
    f' <- hsExp f
    x' <- hsExp x
    return $ H.AppE f' x'

hsExp (LamE _ (Sig n _) x) = do
    x' <- hsExp x
    return $ H.LamE [H.VarP (hsName n)] x'

-- case x of
--    K -> y
--    _ -> n
--
-- Translates to:  __caseK x y n
hsExp (CaseE _ x (Sig kn kt) y n) = do
    [x', y', n'] <- mapM hsExp [x, y, n]
    return $ foldl1 H.AppE [H.VarE (casenm kn), x', y', n']
        
hsType :: Type -> HF H.Type
hsType = hsType' . canonical

hsType' :: Type -> HF H.Type
hsType' t = do
    retype <- asks hfs_retype
    case t of
      _ | Just n <- lookup t retype -> return $ H.VarT (hsName n)
      (ConT n _)
        | n == arrowN -> return H.ArrowT
        | otherwise -> return $ H.ConT (hsTyName n)
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
                    "+" -> H.ConT $ H.mkName "N__PLUS"
                    "-" -> H.ConT $ H.mkName "N__MINUS"
                    "*" -> H.ConT $ H.mkName "N__TIMES"
                    _ -> error $ "hsType' TODO: AppNT " ++ f
        return $ H.AppT (H.AppT f' a') b'
      t -> throw $ "haskellf: unsupported type: " ++ pretty t

-- Return the numeric type corresponding to the given integer.
hsnt :: Integer -> H.Type
hsnt 0 = H.ConT (H.mkName "N__0")
hsnt n = H.AppT (H.ConT (H.mkName $ "N__2p" ++ show (n `mod` 2))) (hsnt $ n `div` 2)

hsTopType :: Context -> Type -> HF H.Type
hsTopType ctx t = do
    (nctx, use) <- mkContext t
    t' <- hsType t
    ctx' <- mapM hsClass ctx
    case nctx ++ ctx' of
        [] -> return t'
        ctx'' -> return $ H.ForallT (map (H.PlainTV . hsName) use) ctx'' t'

hsClass :: Class -> HF H.Pred
hsClass (Class nm ts) = do
    ts' <- mapM hsType ts
    return $ H.ClassP (hsName nm) ts'
    
hsMethod :: Class -> Method -> HF [H.Dec]
hsMethod cls (Method n e) = do
    env <- asks hfs_env
    mt <- lookupMethodType env n cls
    mctx <- lookupMethodContext env n cls
    hsTopExp (TopExp (TopSig n mctx mt) e)


hsSig :: TopSig -> HF H.Dec
hsSig (TopSig n ctx t) = do
    t' <- hsTopType ctx t
    return $ H.SigD (hsName n) t'

hsTopExp :: TopExp -> HF [H.Dec]
hsTopExp (TopExp (TopSig n ctx t) e) = do
 let mkretype :: Type -> [(Type, Name)]
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
            in an `nappend` name opn `nappend` bn
        | NumT i <- t = name ("_" ++ show i)
        | otherwise = error "unexpected type in HaskellF.Compile.retypenm"

 local (\s -> s { hfs_retype = mkretype (canonical t)}) $ do
     t' <- hsTopType ctx t
     e' <- hsExp e
     let hsn = hsName n
     let sig = H.SigD hsn t'
     let val = H.FunD hsn [H.Clause [] (H.NormalB e') []]
     return [sig, val]
    
hsDec :: Dec -> HF [H.Dec]
hsDec (ValD _ e) = hsTopExp e

hsDec (DataD _ n _ _) | n `elem` [
  name "Bool",
  name "Char",
  name "Integer",
  name "Bit",
  name "[]",
  unitN,
  tupleN 2,
  tupleN 3,
  tupleN 4,
  name "Maybe",
  name "SMT",
  name "Symbolic",
  name "Used",
  name "IO"] = return []

hsDec (DataD _ n tyvars constrs) = do
    dataD <- mkDataD n tyvars constrs
    smtenTD <- mkSmtenTD n tyvars
    symbD <- mkSymbD n tyvars constrs
    casesD <- mapM (mkCaseD n tyvars) constrs
    return $ concat ([dataD, smtenTD, symbD] : casesD)

hsDec (ClassD _ ctx n vars exps@(TopExp (TopSig _ _ t) _:_)) = do
    -- Kind inference doesn't currently update the kinds of the vars in the
    -- ClassD declaration, so we look at the vars in one of the method
    -- declarations to figure out the right kind.
    -- TODO: Kind inference should update the vars in the ClassD declaration,
    -- and we should use those here.
    let tvs = filter (flip elem (map tyVarName vars) . fst) (varTs t) 
    (nctx, tyvars) <- mkContext tvs
    local (\s -> s { hfs_tyvars = tyvars}) $ do
        ctx' <- mapM hsClass ctx
        exps' <- mapM hsTopExp exps
        return $ [H.ClassD (nctx ++ ctx') (hsName n) (map (H.PlainTV . hsName) (map tyVarName vars)) [] (concat exps')]

hsDec (InstD _ ctx cls@(Class n ts) ms) = do
    (nctx, tyvars) <- mkContext (appsT (conT n) ts)
    local (\s -> s { hfs_tyvars = tyvars }) $ do
        ctx' <- mapM hsClass ctx
        ms' <- mapM (hsMethod cls) ms
        ts' <- mapM hsType ts
        let t = foldl H.AppT (H.ConT (hsName n)) ts'
        return [H.InstanceD (nctx ++ ctx') t (concat ms')] 

hsDec (PrimD _ s@(TopSig n _ _)) = return []

-- haskell decs
--  Compile the given declarations to haskell.
haskellf ::    Bool     -- ^ Should a "__main" wrapper be generated?
            -> String   -- ^ Name of target module.
            -> Env -> Failable H.Doc
haskellf wrapmain modname env = {-# SCC "HaskellF" #-} do
  let hsHeader :: H.Doc
      hsHeader = H.text "{-# LANGUAGE ExplicitForAll #-}" H.$+$
                 H.text "{-# LANGUAGE MultiParamTypeClasses #-}" H.$+$
                 H.text "{-# LANGUAGE FlexibleInstances #-}" H.$+$
                 H.text "{-# LANGUAGE FlexibleContexts #-}" H.$+$
                 H.text "{-# LANGUAGE UndecidableInstances #-}" H.$+$
                 H.text "{-# LANGUAGE ScopedTypeVariables #-}" H.$+$
                 H.text "{-# LANGUAGE InstanceSigs #-}" H.$+$
                 H.text ("module " ++ modname ++ " where") H.$+$
                 H.text "import qualified Prelude" H.$+$
                 H.text "import qualified Smten.HaskellF.HaskellF as S" H.$+$
                 H.text "import qualified Smten.Name as S" H.$+$
                 H.text "import qualified Smten.Type as S" H.$+$
                 H.text "import qualified Smten.ExpH as S" H.$+$
                 H.text "import Smten.HaskellF.Lib.Prelude" H.$+$
                 H.text "import Smten.HaskellF.Lib.Symbolic" H.$+$
                 H.text "" H.$+$
                 if wrapmain
                    then H.text "__main = __main_wrapper main"
                    else H.empty

      dsm = concat <$> mapM hsDec (getDecls env)
  ds <- runHF env dsm
  return (hsHeader H.$+$ H.ppr ds)

-- Form the context for declarations.
--  t - The type to produce the context for. This is used to identify which
--      variable types to declare.
--  Returns the generated context and list of newly declared type variables.
mkContext :: (VarTs a) => a -> HF ([H.Pred], [Name])
mkContext t = do
  retypes <- map snd <$> asks hfs_retype
  tyvars <- asks hfs_tyvars
  let p = flip notElem tyvars
      vts = filter (p . fst) $ (varTs t ++ [(n, NumK) | n <- retypes])
      tvs = [H.ClassP (nmk "S.HaskellF" k) [H.VarT (hsName n)] | (n, k) <- vts]
  return (tvs, map fst vts)

hsCon :: Con -> HF H.Con
hsCon (Con n tys) = do
    ts <- mapM hsType tys
    return $ H.NormalC (hsName n) (map (\t -> (H.NotStrict, t)) ts)

-- data Foo a b ... =
--    FooA FooA1 FooA2 ...
--  | FooB FooB1 FooB2 ...
--    ...
--  | Foo__s ExpH
mkDataD :: Name -> [TyVar] -> [Con] -> HF H.Dec
mkDataD n tyvars constrs = do
  let tyvars' = map (H.PlainTV . hsName . tyVarName) tyvars
  constrs' <- mapM hsCon constrs
  let sconstr = H.NormalC (symnm n) [(H.NotStrict, H.ConT (H.mkName "S.ExpH"))]
  return $ H.DataD [] (hsName n) tyvars' (constrs' ++ [sconstr]) []

-- Note: we currently don't support crazy kinded instances of SmtenT. This
-- means we are limited to "linear" kinds of the form (* -> * -> ... -> *)
--
-- To handle that properly, we chop off as many type variables as needed to
-- get to a linear kind.
--   call the chopped off type variables c1, c2, ...
--
-- instance (SmtenN c1, SmtenN c2, ...) => SmtenTN (Foo c1 c2 ...) where
--    smtenTN _ = appsT (conT "Foo") [smtenT (undefined :: c1), smtenT (undefined :: c2), ...]
mkSmtenTD :: (MonadError String m) => Name -> [TyVar] -> m H.Dec
mkSmtenTD n tyvars = return $
  let (rkept, rdropped) = span (\(TyVar n k) -> knum k == 0) (reverse tyvars)
      nkept = genericLength rkept
      dropped = reverse rdropped
      ctx = [H.ClassP (nmk "S.SmtenT" k) [H.VarT (hsName n)] | TyVar n k <- dropped]
      cont = H.AppE (H.VarE (H.mkName "S.conT"))
                    (H.AppE (H.VarE (H.mkName "S.name"))
                            (H.LitE (H.StringL (unname n))))

      mkarg :: TyVar -> H.Exp
      mkarg (TyVar n k) = 
        H.AppE (H.VarE (nmk "S.smtenT" k))
               (H.SigE (H.VarE (H.mkName "Prelude.undefined"))
                       (foldl H.AppT (H.VarT (hsName n)) (genericReplicate (knum k) (H.ConT (H.mkName "()")))))

      args = H.ListE (map mkarg dropped)
      body = H.AppE (H.AppE (H.VarE (H.mkName "S.appsT")) cont) args
      smtent = H.FunD (nmn "smtenT" nkept) [
                H.Clause [H.WildP] (H.NormalB body) []]
      tyt = H.AppT (H.ConT $ nmn "S.SmtenT" nkept)
                   (foldl H.AppT (H.ConT (hsName n)) [H.VarT (hsName n) | TyVar n _ <- dropped])
  in H.InstanceD ctx tyt [smtent]
  
-- instance S.HaskellFN Foo where
--  boxN ...
--  unboxN ...
--
-- Note: we do the same thing with crazy kinds as mkSmtenTD
mkSymbD :: (MonadError String m) => Name -> [TyVar] -> [Con] -> m H.Dec
mkSymbD n tyvars constrs = do
    let (rkept, rdropped) = span (\(TyVar n k) -> knum k == 0) (reverse tyvars)
        nkept = genericLength rkept
        dropped = reverse rdropped
    boxD <- mkBoxD n nkept constrs
    unboxD <- mkUnboxD n nkept constrs
    let ctx = [H.ClassP (nmk "S.HaskellF" k) [H.VarT (hsName n)] | TyVar n k <- dropped]
        clsname = nmn "S.HaskellF" nkept
        ty = H.AppT (H.ConT clsname) 
                    (foldl H.AppT (H.ConT (hsName n)) [H.VarT (hsName n) | TyVar n _ <- dropped])
    return $ H.InstanceD ctx ty [boxD, unboxD]

--  boxN e
--   | Just [a, b, ...] <- de_conHF "FooA" e = FooA (box a) (box b) ...
--   | Just [a, b, ...] <- de_conHF "FooB" e = FooB (box a) (box b) ...
--   ...
--   | otherwise = Foo__s e
mkBoxD :: (MonadError String m) => Name -> Integer -> [Con] -> m H.Dec
mkBoxD n bn constrs = do
  let mkGuard :: Con -> (H.Guard, H.Exp)
      mkGuard (Con cn tys) = 
        let argnms = [H.mkName ("x" ++ show i) | i <- [1..length tys]]
            pat = H.ConP (H.mkName "Prelude.Just") [H.ListP (map H.VarP argnms)]
            src = foldl1 H.AppE [
                    H.VarE (H.mkName "S.de_conHF"),
                    H.LitE (H.StringL (unname cn)),
                    H.VarE (H.mkName "e")]
            guard = H.PatG [H.BindS pat src]
            boxes = [H.AppE (H.VarE (H.mkName "S.box")) (H.VarE an) | an <- argnms]
            body = foldl H.AppE (H.ConE (hsName cn)) boxes
        in (guard, body)

      sguard = (H.NormalG (H.VarE (H.mkName "Prelude.otherwise")), H.AppE (H.ConE (symnm n)) (H.VarE (H.mkName "e")))
      guards = map mkGuard constrs ++ [sguard]
      clause = H.Clause [H.VarP (H.mkName "e")] (H.GuardedB guards) []
  return $ H.FunD (nmn "box" bn) [clause]
  

--  unboxN x
--   | FooA a b ... <- x = conHF x "FooA" [unbox a, unbox b, ...]
--   | FooB a b ... <- x = conHF x "FooB" [unbox a, unbox b, ...]
--   ...
--   | Foo__s v <- x = v
mkUnboxD :: (MonadError String m) => Name -> Integer -> [Con] -> m H.Dec
mkUnboxD n bn constrs = do
    let mkGuard :: Con -> (H.Guard, H.Exp)
        mkGuard (Con cn tys) =
          let argnms = [H.mkName ("x" ++ show i) | i <- [1..length tys]]
              pat = H.ConP (hsName cn) (map H.VarP argnms)
              src = H.VarE (H.mkName "x")
              guard = H.PatG [H.BindS pat src]
              unboxes = [H.AppE (H.VarE (H.mkName "S.unbox")) (H.VarE an) | an <- argnms]
              body = foldl1 H.AppE [
                        H.VarE (H.mkName "S.conHF"),
                        H.VarE (H.mkName "x"),
                        H.LitE (H.StringL (unname cn)),
                        H.ListE unboxes]
          in (guard, body)

        spat = H.ConP (symnm n) [H.VarP (H.mkName "v")]
        ssrc = H.VarE (H.mkName "x")
        sguard = (H.PatG [H.BindS spat ssrc], H.VarE (H.mkName "v"))
        guards = map mkGuard constrs ++ [sguard]
        clause = H.Clause [H.VarP (H.mkName "x")] (H.GuardedB guards) []
    return $ H.FunD (nmn "unbox" bn) [clause]

-- __caseFooB :: Foo -> (FooB1 -> FooB2 -> ... -> z) -> z -> z
-- __caseFooB x y n
--   | FooB a b ... <- x = y a b ...
--   | Foo__s _ <- x = caseHF "FooB" x y n
--   | otherwise = n
mkCaseD :: Name -> [TyVar] -> Con -> HF [H.Dec]
mkCaseD n tyvars (Con cn tys) = do
  let dt = appsT (conT n) (map tyVarType tyvars)
      z = VarT (name "z") StarK
      t = arrowsT [dt, arrowsT (tys ++ [z]), z, z]
  ht <- hsTopType [] t
  let sigD = H.SigD (casenm cn) ht

      body = H.AppE (H.VarE (H.mkName "S.caseHF")) (H.LitE (H.StringL (unname cn)))

      yargs = [H.mkName ("x" ++ show i) | i <- [1..length tys]]
      ypat = H.ConP (hsName cn) (map H.VarP yargs)
      ysrc = H.VarE (H.mkName "x")
      ybody = foldl H.AppE (H.VarE (H.mkName "y")) [H.VarE an | an <- yargs]
      yguard = (H.PatG [H.BindS ypat ysrc], ybody)

      spat = H.ConP (symnm n) [H.WildP]
      ssrc = H.VarE (H.mkName "x")
      sbody = foldl1 H.AppE [
                H.VarE (H.mkName "S.caseHF"),
                H.LitE (H.StringL (unname cn)),
                H.VarE (H.mkName "x"),
                H.VarE (H.mkName "y"),
                H.VarE (H.mkName "n")]
      sguard = (H.PatG [H.BindS spat ssrc], sbody)

      nguard = (H.NormalG (H.VarE (H.mkName "Prelude.otherwise")), H.VarE (H.mkName "n"))

      guards = [yguard, sguard, nguard]
      clause = H.Clause [H.VarP (H.mkName [c]) | c <- "xyn"] (H.GuardedB guards) []
      funD = H.FunD (casenm cn) [clause]
  return [sigD, funD]

