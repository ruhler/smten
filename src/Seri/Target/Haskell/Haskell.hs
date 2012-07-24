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

module Seri.Target.Haskell.Haskell (
    haskell, haskellH,
    ) where

import Data.List(nub)
import Data.Maybe(fromJust)

import qualified Language.Haskell.TH.PprLib as H
import qualified Language.Haskell.TH as H

import Seri.Failable
import Seri.Lambda
import Seri.Target.Haskell.Compiler
import Seri.Target.Haskell.Builtins.Prelude

hsExp :: HCompiler -> Exp -> Failable H.Exp
hsExp c (LitE (IntegerL i)) = do
    t <- compile_type c c integerT
    return $ H.SigE (H.LitE (H.IntegerL i)) t
hsExp c (LitE (CharL i)) = return (H.LitE (H.CharL i))
hsExp c (CaseE e ms) = do
    e' <- compile_exp c c e
    ms' <- mapM (hsMatch c) ms
    return $ H.CaseE e' ms'
hsExp c (AppE f x) = do
    f' <- compile_exp c c f
    x' <- compile_exp c c x
    return $ H.AppE f' x'
hsExp c (LamE (Sig n _) x) = do
    x' <- compile_exp c c x
    return $ H.LamE [H.VarP (hsName n)] x'
hsExp c (ConE (Sig n _)) = return $ H.ConE (hsName n)
hsExp c (VarE (Sig n _)) = return $ H.VarE (hsName n)

hsMatch :: HCompiler -> Match -> Failable H.Match
hsMatch c (Match p e) = do
    let p' = hsPat p
    e' <- compile_exp c c e
    return $ H.Match p' (H.NormalB $ e') []
    
hsPat :: Pat -> H.Pat
hsPat (ConP _ n ps) = H.ConP (hsName n) (map hsPat ps)
hsPat (VarP (Sig n _)) = H.VarP (hsName n)
hsPat (IntegerP i) = H.LitP (H.IntegerL i)
hsPat (WildP _) = H.WildP

hsType :: HCompiler -> Type -> Failable H.Type
hsType c (ConT "->") = return H.ArrowT
hsType c (ConT n) = return $ H.ConT (hsName n)
hsType c (AppT a b) = do
    a' <- compile_type c c a
    b' <- compile_type c c b
    return $ H.AppT a' b'
hsType c (VarT n) = return $ H.VarT (hsName n)
hsType c (NumT (ConNT i)) = return $ hsnt i
hsType c (NumT (VarNT n)) = return $ H.VarT (H.mkName n)
hsType c (NumT (AppNT f a b)) = do
    a' <- hsType c (NumT a)
    b' <- hsType c (NumT b)
    let f' = case f of
                "+" -> H.ConT $ H.mkName "N__PLUS"
                "*" -> H.ConT $ H.mkName "N__TIMES"
                _ -> error $ "hsType TODO: AppNT " ++ f
    return $ H.AppT (H.AppT f' a') b'
hsType c t = fail $ "coreH does not apply to type: " ++ pretty t

-- Return the numeric type corresponding to the given integer.
hsnt :: Integer -> H.Type
hsnt 0 = H.ConT (H.mkName "N__0")
hsnt n = H.AppT (H.ConT (H.mkName $ "N__2p" ++ show (n `mod` 2))) (hsnt $ n `div` 2)

hsTopType :: HCompiler -> Context -> Type -> Failable H.Type
hsTopType c ctx t = do
    let ntvs = [H.ClassP (H.mkName "N__") [H.VarT (H.mkName n)] | n <- nvarTs t]
    t' <- compile_type c c t
    ctx' <- mapM (hsClass c) ctx
    case ntvs ++ ctx' of
        [] -> return t'
        ctx'' -> return $ H.ForallT (map (H.PlainTV . H.mkName) (nvarTs t ++ varTs t)) ctx'' t'

hsClass :: HCompiler -> Class -> Failable H.Pred
hsClass c (Class nm ts) = do
    ts' <- mapM (compile_type c c) ts
    return $ H.ClassP (hsName nm) ts'
    
hsMethod :: HCompiler -> Method -> Failable H.Dec
hsMethod c (Method n e) = do
    let hsn = hsName n
    e' <- compile_exp c c e
    return $ H.ValD (H.VarP hsn) (H.NormalB e') []


hsCon :: HCompiler -> Con -> Failable H.Con
hsCon c (Con n tys) = do
    ts <- mapM (compile_type c c) tys
    return $ H.NormalC (hsName n) (map (\t -> (H.NotStrict, t)) ts)
    
hsSig :: HCompiler -> TopSig -> Failable H.Dec
hsSig c (TopSig n ctx t) = do
    t' <- hsTopType c ctx t
    return $ H.SigD (hsName n) t'

    
hsDec :: HCompiler -> Dec -> Failable [H.Dec]
hsDec c (ValD (TopSig n ctx t) e) = do
    t' <- hsTopType c ctx t
    e' <- compile_exp c c e
    let hsn = hsName n
    let sig = H.SigD hsn t'
    let val = H.FunD hsn [H.Clause [] (H.NormalB e') []]
    return [sig, val]

hsDec c (DataD n tyvars constrs) = do
    cs <- mapM (hsCon c) constrs
    return [H.DataD [] (hsName n) (map (H.PlainTV . hsName . tyVarName) tyvars) cs []]

hsDec c (ClassD n vars sigs) = do
    sigs' <- mapM (hsSig c) sigs
    return $ [H.ClassD [] (hsName n) (map (H.PlainTV . hsName . tyVarName) vars) [] sigs']

hsDec c (InstD ctx (Class n ts) ms) = do
    ctx' <- mapM (hsClass c) ctx
    ms' <- mapM (hsMethod c) ms
    ts' <- mapM (compile_type c c) ts
    let t = foldl H.AppT (H.ConT (hsName n)) ts'
    return [H.InstanceD ctx' t ms'] 

hsDec _ d = fail $ "coreH does not apply to dec: " ++ pretty d

coreH :: HCompiler
coreH = Compiler hsExp hsType hsDec

haskellH :: HCompiler
haskellH = compilers [preludeH, coreH]

-- haskell builtin decs
--  Compile the given declarations to haskell.
haskell :: HCompiler -> Env -> Name -> H.Doc
haskell c env main =
  let hsHeader :: H.Doc
      hsHeader = H.text "{-# LANGUAGE ExplicitForAll #-}" H.$+$
                 H.text "{-# LANGUAGE MultiParamTypeClasses #-}" H.$+$
                 H.text "{-# LANGUAGE FlexibleInstances #-}" H.$+$
                 H.text "import qualified Prelude" H.$+$
                 H.text "import Seri.Target.Haskell.Lib.Numeric"

      ds = compile_decs c env
  in hsHeader H.$+$ H.ppr ds H.$+$
        H.text "main :: Prelude.IO ()" H.$+$
        H.text "main = Prelude.putStrLn (case "
        H.<+> H.text main H.<+> H.text " of { True -> \"True\"; False -> \"False\"})"

