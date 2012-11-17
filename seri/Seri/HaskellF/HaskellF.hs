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

{-# LANGUAGE PatternGuards #-}

-- Back end target which translates seri programs into Haskell. Supports the
-- Query monad and SMT queries.
module Seri.HaskellF.HaskellF (
    haskellf,
    ) where

import Debug.Trace

import Data.Char(isAlphaNum)
import Data.Functor((<$>))
import Data.List(nub)
import Data.Maybe(fromJust)

import qualified Language.Haskell.TH.PprLib as H
import qualified Language.Haskell.TH as H

import Seri.Failable
import Seri.Name
import Seri.Sig
import Seri.Type
import Seri.Lit
import Seri.Exp
import Seri.Dec
import Seri.Ppr

-- TODO: Here we just drop the qualified part of the name.
-- This is a hack, requiring there are no modules which define an entity of
-- the same name (unlikely...). Really we should form a proper haskell name
-- for whatever this name is used for (varid, conid)
hsName :: Name -> H.Name
hsName =
  let dequalify :: String -> String
      dequalify n = 
        case break (== '.') n of
            (n', []) -> n'
            (_, ".") -> "."
            (_, n') -> dequalify (tail n')
      symify :: String -> String
      symify s = if issymbol s then "(" ++ s ++ ")" else s
  in H.mkName . symify . dequalify . unname

issymbol :: String -> Bool
issymbol ('(':_) = False
issymbol "[]" = False
issymbol (h:_) = not $ isAlphaNum h || h == '_'


hsLit :: Lit -> H.Lit
hsLit (IntegerL i) = H.IntegerL i
hsLit (CharL c) = H.CharL c

hsExp :: Exp -> Failable H.Exp

-- String literals:
-- TODO: the template haskell pretty printer doesn't print strings correctly
-- if they contain newlines, thus, we can't print those as string literals.
-- When they fix the template haskell pretty printer, that special case should
-- be removed here.
hsExp e | Just str <- de_stringE e
          , '\n' `notElem` str
          = return $ H.LitE (H.StringL str)
hsExp e | Just xs <- de_listE e = do
  xs' <- mapM hsExp xs
  return $ H.ListE xs'

hsExp (LitE l) = return (H.LitE (hsLit l))
hsExp (ConE (Sig n _)) = return $ H.ConE (hsName n)
hsExp (VarE (Sig n t)) | unknowntype t = return $ H.VarE (hsName n)
hsExp (VarE (Sig n t)) = do
    -- Give explicit type signature to make sure there are no type ambiguities
    ht <- hsType t
    return $ H.SigE (H.VarE (hsName n)) ht
hsExp (AppE f x) = do
    f' <- hsExp f
    x' <- hsExp x
    return $ H.AppE f' x'

hsExp (LamE (Sig n _) x) = do
    x' <- hsExp x
    return $ H.LamE [H.VarP (hsName n)] x'

hsExp (CaseE x (Sig kn kt) y n) =
  let nwant = length (de_arrowsT kt) - 1
      getvars :: Int -> Exp -> ([Name], Exp)
      getvars 0 x = ([], x)
      getvars n (LamE (Sig nm _) x) = 
        let (vs, b) = getvars (n-1) x
        in (nm:vs, b)
      getvars _ e = error $ "getvars expected LamE, found: " ++ pretty e

      (vars, yv) = getvars nwant y
  in do
    x' <- hsExp x
    yv' <- hsExp yv
    n' <- hsExp n
    return $ H.CaseE x' [
        H.Match (H.ConP (hsName kn) (map (H.VarP . hsName) vars)) (H.NormalB yv') [],
        H.Match H.WildP (H.NormalB n') []]
    
    
hsType :: Type -> Failable H.Type
hsType (ConT n) | n == name "Char" = return $ H.ConT (H.mkName "Prelude.Char")
hsType (ConT n) | n == name "Integer" = return $ H.ConT (H.mkName "Prelude.Integer")
hsType (ConT n) | n == name "IO" = return $ H.ConT (H.mkName "Prelude.IO")
hsType (ConT n) | n == name "->" = return H.ArrowT
hsType (ConT n) = return $ H.ConT (hsName n)
hsType (AppT a b) = do
    a' <- hsType a
    b' <- hsType b
    return $ H.AppT a' b'
hsType (VarT n) = return $ H.VarT (hsName n)
hsType (NumT (ConNT i)) = return $ hsnt i
hsType (NumT (VarNT n)) = return $ H.VarT (H.mkName (pretty n))
hsType (NumT (AppNT f a b)) = do
    a' <- hsType (NumT a)
    b' <- hsType (NumT b)
    let f' = case f of
                "+" -> H.ConT $ H.mkName "N__PLUS"
                "-" -> H.ConT $ H.mkName "N__MINUS"
                "*" -> H.ConT $ H.mkName "N__TIMES"
                _ -> error $ "hsType TODO: AppNT " ++ f
    return $ H.AppT (H.AppT f' a') b'
hsType t = throw $ "coreH does not apply to type: " ++ pretty t

-- Return the numeric type corresponding to the given integer.
hsnt :: Integer -> H.Type
hsnt 0 = H.ConT (H.mkName "N__0")
hsnt n = H.AppT (H.ConT (H.mkName $ "N__2p" ++ show (n `mod` 2))) (hsnt $ n `div` 2)

hsTopType :: Context -> Type -> Failable H.Type
hsTopType ctx t = do
    let ntvs = [H.ClassP (H.mkName "N__") [H.VarT (H.mkName (pretty n))] | n <- nvarTs t]
    t' <- hsType t
    ctx' <- mapM hsClass ctx
    case ntvs ++ ctx' of
        [] -> return t'
        ctx'' -> return $ H.ForallT (map (H.PlainTV . H.mkName . pretty) (nvarTs t ++ varTs t)) ctx'' t'

hsClass :: Class -> Failable H.Pred
hsClass (Class nm ts) = do
    ts' <- mapM hsType ts
    return $ H.ClassP (hsName nm) ts'
    
hsMethod :: Method -> Failable H.Dec
hsMethod (Method n e) = do
    let hsn = hsName n
    e' <- hsExp e
    return $ H.ValD (H.VarP hsn) (H.NormalB e') []


hsCon :: Con -> Failable H.Con
hsCon (Con n tys) = do
    ts <- mapM hsType tys
    return $ H.NormalC (hsName n) (map (\t -> (H.NotStrict, t)) ts)
    
hsSig :: TopSig -> Failable H.Dec
hsSig (TopSig n ctx t) = do
    t' <- hsTopType ctx t
    return $ H.SigD (hsName n) t'

    
hsDec :: Dec -> Failable [H.Dec]
hsDec (ValD (TopSig n ctx t) e) = do
    t' <- hsTopType ctx t
    e' <- hsExp e
    let hsn = hsName n
    let sig = H.SigD hsn t'
    let val = H.FunD hsn [H.Clause [] (H.NormalB e') []]
    return [sig, val]

hsDec (DataD n _ _) | n `elem` [
  name "Char",
  name "Integer",
  name "()",
  name "(,)",
  name "(,,)",
  name "(,,,)",
  name "(,,,,)",
  name "(,,,,,)",
  name "(,,,,,,)",
  name "(,,,,,,,)",
  name "(,,,,,,,,)",
  name "[]",
  name "Bit",
  name "IO"] = return []

hsDec (DataD n tyvars constrs) = do
    cs <- mapM hsCon constrs
    return [H.DataD [] (hsName n) (map (H.PlainTV . hsName . tyVarName) tyvars) cs []]

hsDec (ClassD n vars sigs) = do
    sigs' <- mapM hsSig sigs
    return $ [H.ClassD [] (hsName n) (map (H.PlainTV . hsName . tyVarName) vars) [] sigs']

hsDec (InstD ctx (Class n ts) ms) = do
    let ntvs = [H.ClassP (H.mkName "N__") [H.VarT (H.mkName (pretty n))] | n <- concat $ map nvarTs ts]
    ctx' <- mapM hsClass ctx
    ms' <- mapM hsMethod ms
    ts' <- mapM hsType ts
    let t = foldl H.AppT (H.ConT (hsName n)) ts'
    return [H.InstanceD (ntvs ++ ctx') t ms'] 

hsDec (PrimD (TopSig n _ t)) | n == name "Prelude.error" = do
  let e = H.VarE (H.mkName "Prelude.error")
  let val = H.FunD (H.mkName "error") [H.Clause [] (H.NormalB e) []]
  return [val]

hsDec (PrimD s@(TopSig n _ _))
 | n == name "Prelude.__prim_add_Integer" = prim s (vare "Prelude.+")
 | n == name "Prelude.__prim_sub_Integer" = prim s (vare "Prelude.-")
 | n == name "Prelude.__prim_mul_Integer" = prim s (vare "Prelude.*")
 | n == name "Prelude.__prim_show_Integer" = prim s (vare "Prelude.show")
 | n == name "Prelude.<" = bprim s "Prelude.<"
 | n == name "Prelude.<=" = bprim s "Prelude.<="
 | n == name "Prelude.>" = bprim s "Prelude.>"
 | n == name "Prelude.&&" = prim s (vare "&&#")
 | n == name "Prelude.||" = prim s (vare "||#")
 | n == name "Prelude.not" = prim s (vare "not_")
 | n == name "Prelude.__prim_eq_Integer" = bprim s "Prelude.=="
 | n == name "Prelude.__prim_eq_Char" = bprim s "Prelude.=="
 | n == name "Prelude.valueof" = return []
 | n == name "Prelude.numeric" = return []
 | n == name "Seri.Bit.__prim_fromInteger_Bit" = prim s (vare "Prelude.fromInteger")
 | n == name "Seri.Bit.__prim_eq_Bit" = bprim s "Prelude.=="
 | n == name "Seri.Bit.__prim_add_Bit" = prim s (vare "Prelude.+")
 | n == name "Seri.Bit.__prim_sub_Bit" = prim s (vare "Prelude.-")
 | n == name "Seri.Bit.__prim_mul_Bit" = prim s (vare "Prelude.*")
 | n == name "Seri.Bit.__prim_concat_Bit" = prim s (vare "Bit.concat")
 | n == name "Seri.Bit.__prim_show_Bit" = prim s (vare "Prelude.show")
 | n == name "Seri.Bit.__prim_not_Bit" = prim s (vare "Bit.not")
 | n == name "Seri.Bit.__prim_or_Bit" = prim s (vare "Bit.or")
 | n == name "Seri.Bit.__prim_and_Bit" = prim s (vare "Bit.and")
 | n == name "Seri.Bit.__prim_shl_Bit" = prim s (vare "Bit.shl")
 | n == name "Seri.Bit.__prim_lshr_Bit" = prim s (vare "Bit.lshr")
 | n == name "Seri.Bit.__prim_zeroExtend_Bit" = prim s (vare "Bit.zeroExtend")
 | n == name "Seri.Bit.__prim_truncate_Bit" = prim s (vare "Bit.truncate")
 | n == name "Seri.Bit.__prim_extract_Bit" = prim s (vare "Bit.extract")
 | n == name "Prelude.return_io" = prim s (vare "Prelude.return")
 | n == name "Prelude.bind_io" = prim s (vare "Prelude.>>=")
 | n == name "Prelude.nobind_io" = prim s (vare "Prelude.>>")
 | n == name "Prelude.fail_io" = prim s (vare "Prelude.fail")
 | n == name "Prelude.putChar" = prim s (vare "Prelude.putChar")

hsDec d = throw $ "coreH does not apply to dec: " ++ pretty d

-- haskell decs
--  Compile the given declarations to haskell.
haskellf :: [Dec] -> Name -> H.Doc
haskellf env main =
  let hsHeader :: H.Doc
      hsHeader = H.text "{-# LANGUAGE ExplicitForAll #-}" H.$+$
                 H.text "{-# LANGUAGE MultiParamTypeClasses #-}" H.$+$
                 H.text "{-# LANGUAGE FlexibleInstances #-}" H.$+$
                 H.text "import qualified Prelude" H.$+$
                 H.text "import Seri.Haskell.Lib.Numeric" H.$+$
                 H.text "import qualified Seri.Haskell.Lib.Bit as Bit" H.$+$
                 H.text "import Seri.Haskell.Lib.Bit(Bit)"

      ds = surely $ (concat <$> mapM hsDec env)
  in hsHeader H.$+$ H.ppr ds H.$+$
        H.text "main :: Prelude.IO ()" H.$+$
        H.text "main = " H.<+> H.text (pretty main)

-- | Declare a primitive seri implemented with the given haskell expression.
prim :: TopSig -> H.Exp -> Failable [H.Dec]
prim s@(TopSig nm ctx t) b = do
  let hsn = hsName nm
  sig <- hsSig s
  let val = H.FunD hsn [H.Clause [] (H.NormalB b) []]
  return [sig, val]

vare :: String -> H.Exp
vare n = H.VarE (H.mkName n)

-- | Declare a binary predicate primitive in haskell.
bprim :: TopSig -> String -> Failable [H.Dec]
bprim s@(TopSig nm _ t) b = do    
  let hsn = hsName nm
  sig <- hsSig s
  let val = H.FunD hsn [H.Clause
          [H.VarP (H.mkName "a"), H.VarP (H.mkName "b")]
              (H.NormalB (
                  H.CondE (H.AppE (H.AppE (vare b) (vare "a")) (vare "b"))
                          (H.ConE (H.mkName "True"))
                          (H.ConE (H.mkName "False"))
              )) []]
  return [sig, val]

unknowntype :: Type -> Bool
unknowntype (ConT {}) = False
unknowntype (AppT a b) = unknowntype a || unknowntype b
unknowntype (VarT {}) = True
unknowntype (NumT {}) = True    -- TODO: this may not be unknown, right?
unknowntype UnknownT = True


