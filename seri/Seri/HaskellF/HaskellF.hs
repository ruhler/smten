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
import qualified Language.Haskell.TH.Syntax as H

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
hsName n =
  let dequalify :: String -> String
      dequalify n = 
        case break (== '.') n of
            (n', []) -> n'
            (_, ".") -> "."
            (_, n') -> dequalify (tail n')
      symify :: String -> String
      symify s = if issymbol s then "(" ++ s ++ ")" else s
  in H.mkName . symify . dequalify . unname $ n

issymbol :: String -> Bool
issymbol ('(':_) = False
issymbol "[]" = False
issymbol (h:_) = not $ isAlphaNum h || h == '_'


hsLit :: Lit -> H.Lit
hsLit (IntegerL i) = H.IntegerL i
hsLit (CharL c) = H.CharL c

prependnm :: String -> Name -> H.Name
prependnm m n = hsName $ name m `nappend` n

-- Given the name of a data constructor, return the name of the corresponding
-- abstract constructor function.
constrnm :: Name -> H.Name
constrnm = prependnm "__mk"

constrisnm :: Name -> H.Name
constrisnm = prependnm "__is"

constrappnm :: Name -> H.Name
constrappnm = prependnm "__app"

constrtagnm :: Name -> H.Name
constrtagnm = prependnm "__t"

constrvalnm :: Int -> Name -> H.Name
constrvalnm i = prependnm $ "__v" ++ show i

-- Given the name of a data constructor, return the name of the function for
-- doing a case match against the constructor.
constrcasenm :: Name -> H.Name
constrcasenm n 
 | n == name "()" = constrcasenm $ name "Unit__"
 | Just x <- de_tupleN n = constrcasenm . name $ "Tuple" ++ show x ++ "__"
 | n == name "[]" = constrcasenm $ name "Nil__"
 | n == name ":" = constrcasenm $ name "Cons__"
constrcasenm n = prependnm "__case" n

hsExp :: Exp -> Failable H.Exp
hsExp (LitE l) = return (H.LitE (hsLit l))
hsExp (ConE (Sig n t))
  | n == name "()" = hsExp (ConE (Sig (name "Unit__") t))
  | Just x <- de_tupleN n = hsExp (ConE (Sig (name $ "Tuple" ++ show x ++ "__") t))
  | n == name ":" = hsExp (ConE (Sig (name "Cons__") t))
  | n == name "[]" = hsExp (ConE (Sig (name "Nil__") t))
hsExp (ConE (Sig n _)) = return $ H.VarE (constrnm n)
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

-- case x of
--    K -> y
--    _ -> n
--
-- Translates to:  __caseK x y n
hsExp (CaseE x (Sig kn kt) y n) = do
    [x', y', n'] <- mapM hsExp [x, y, n]
    return $ foldl1 H.AppE [H.VarE (constrcasenm kn), x', y', n']
        
hsType :: Type -> Failable H.Type
hsType (ConT n) | n == name "()" = return $ H.ConT (H.mkName "Unit__")
hsType (ConT n) | Just x <- de_tupleN n
  = return $ H.ConT (H.mkName $ "Tuple" ++ show x ++ "__")
hsType (ConT n) | n == name "[]" = return $ H.ConT (H.mkName "List__")
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
  name "Bool",
  name "Char",
  name "Integer",
  name "Bit",
  name "[]",
  name "()",
  name "IO"] = return []

hsDec (DataD n _ _) | Just x <- de_tupleN n = hsDec $ tuple (fromIntegral x)

-- data Foo a b ... = FooA FooA1 FooA2 ...
--                  | FooB FooB1 FooB2 ...
--                  ...
--
-- Translates to:
-- data Foo a b ... = Foo {
--   __tFooA :: Bool,
--   __vFooA1 :: FooA1,
--   __vFooA2 :: FooA2,
--   ...
--
--   __tFooB :: Bool,
--   __vFooB1 :: FooB1,
--   __vFooB2 :: FooB2,
--   ...
--   }
--
-- instance (Symbolic__ a, Symbolic__ b, ...) => Symbolic__ Foo where
--  __if p a b = Foo {
--      __tFooA = __if p (__tFooA a) (__tFooA b)
--      __vFooA1 = __if p (__vFooA1 a) (__vFooA1 b)
--      ...
--   }
-- __mkFooB :: FooB1 -> FooB2 -> ... -> Foo
-- __mkFooB b1 b2 ... = Foo {
--      __tFooA = False,
--      __tFooB = True,
--      __vFooB1 = b1,
--      __vFooB2 = b2,
--      ...
--   }
-- __isFooB :: Foo -> Bool
-- __isFooB f = and [not (__tFooA f), __tFooB f]
-- __appFooB :: Foo -> (FooB1 -> FooB2 -> ... -> a) -> a
-- __appFooB x f = f (__vFooB1 x) (__vFooB2 x) ...
-- __caseFooB :: Foo -> (FooB1 -> FooB2 -> ... -> a) -> a -> a
-- __caseFooB x y n = __if (__isFooB x) (__appFooB x y) n
-- ...
hsDec (DataD n tyvars constrs) =
  let -- Make the record constructor fields for the given the seri constructor.
      mkrconfs :: Con -> Failable [H.VarStrictType]
      mkrconfs (Con cn cts) = do
        bt <- hsType boolT
        cts' <- mapM hsType cts
        let tag = (constrtagnm cn, H.NotStrict, bt)
        let vals = [(constrvalnm i cn, H.NotStrict, t) | (t, i) <- zip cts' [1..]]
        return (tag:vals)

      -- Make the record constructor with all the fields.
      mkrcon :: [Con] -> Failable H.Con
      mkrcon cs = do
        fields <- concat <$> mapM mkrconfs cs
        return $ H.RecC (hsName n) fields

      -- Make the instance of Symbolic__
      mkinst :: H.Con -> H.Dec
      mkinst (H.RecC cn fields) =
        let clsname = (H.mkName "Symbolic__")
            varts = [H.VarT (hsName (tyVarName v)) | v <- tyvars]
            ctx = [H.ClassP clsname [v] | v <- varts]
            ty = H.AppT (H.ConT clsname) (foldl H.AppT (H.ConT cn) varts)

            iffield :: H.VarStrictType -> H.FieldExp
            iffield (n, _, _) = 
                let e = foldl H.AppE (H.VarE (H.mkName "__if")) [
                            H.VarE (H.mkName "p"),
                            H.AppE (H.VarE n) (H.VarE (H.mkName "a")),
                            H.AppE (H.VarE n) (H.VarE (H.mkName "b"))]
                in (n, e)

            body = H.NormalB $ H.RecConE cn (map iffield fields)
            ifmethod = H.FunD (H.mkName "__if") [
                H.Clause [H.VarP (H.mkName x) | x <- ["p", "a", "b"]] body []
                ]
        in H.InstanceD ctx ty [ifmethod]

      mkmk :: [Name] -> Name -> [H.Type] -> H.Dec
      mkmk prev cn ctys =
        let argnms = [H.mkName $ "x" ++ show i | i <- [1..(length ctys)]]
            oldts = [(constrtagnm p, H.VarE (constrnm (name "False"))) | p <- prev]
            thists = [(constrtagnm cn, H.VarE (constrnm (name "True")))]
            thisvs = [(constrvalnm i cn, H.VarE a) | (a, i) <- zip argnms [1..]]
            body = H.NormalB $ H.RecConE (hsName n) (concat [oldts, thists, thisvs])
        in H.FunD (constrnm cn) [H.Clause (map H.VarP argnms) body []]

      mkis :: [Name] -> Name -> [H.Type] -> H.Dec
      mkis prev cn _ =
        let argnm = H.mkName $ "x"
            oldts = [H.AppE (H.VarE $ H.mkName "not") (
                        H.AppE (H.VarE $ constrtagnm p) (H.VarE argnm))
                          | p <- prev]
            thist = H.AppE (H.VarE $ constrtagnm cn) (H.VarE argnm)
            l = H.ListE $ oldts ++ [thist]
            body = H.NormalB $ H.AppE (H.VarE $ H.mkName "and") l
        in H.FunD (constrisnm cn) [H.Clause [H.VarP argnm] body []]

      mkapp :: [Name] -> Name -> [H.Type] -> H.Dec
      mkapp _ cn ctys = 
        let xarg = H.mkName "x"
            farg = H.mkName "f"
            vs = [H.VarE (constrvalnm i cn) | i <- [1..(length ctys)]]
            app = foldl H.AppE (H.VarE farg) [H.AppE v (H.VarE xarg) | v <- vs]
            body = H.NormalB app
        in H.FunD (constrappnm cn) [H.Clause [H.VarP xarg, H.VarP farg] body []]

      mkcase :: [Name] -> Name -> [H.Type] -> H.Dec
      mkcase _ cn _ = 
        let [x, y, n] = map H.mkName ["x", "y", "n"]
            app = foldl H.AppE (H.VarE $ H.mkName "__if") [
                      H.AppE (H.VarE (constrisnm cn)) (H.VarE x),
                      H.AppE (H.AppE (H.VarE (constrappnm cn)) (H.VarE x)) (H.VarE y),
                      H.VarE n]
            body = H.NormalB app
        in H.FunD (constrcasenm cn) [H.Clause (map H.VarP [x, y, n]) body []]

      mkconfs :: [Name] -> Con -> Failable [H.Dec]
      mkconfs prev (Con cn ctys) = do
        htys <- mapM hsType ctys
        return $ [f prev cn htys | f <- [mkmk, mkis, mkapp, mkcase]]

      mkallconfs :: [Con] -> Failable [H.Dec]
      mkallconfs [] = return []
      mkallconfs (x:xs) = do
          xd <- mkconfs [n | Con n _ <- xs] x
          xds <- mkallconfs xs
          return (xd ++ xds)

  in do
    con <- mkrcon constrs
    confs <- mkallconfs constrs
    let tyvars' = map (H.PlainTV . hsName . tyVarName) tyvars
        n' = hsName n
        dataD = H.DataD [] n' tyvars' [con] []
        instD = mkinst con
    return $ concat [[dataD, instD], confs]

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

hsDec (PrimD s@(TopSig n _ _))
 | n == name "Prelude.__prim_add_Integer" = return []
 | n == name "Prelude.__prim_sub_Integer" = return []
 | n == name "Prelude.__prim_mul_Integer" = return []
 | n == name "Prelude.__prim_show_Integer" = return []
 | n == name "Prelude.<" = return []
 | n == name "Prelude.<=" = return []
 | n == name "Prelude.>" = return []
 | n == name "Prelude.&&" = return []
 | n == name "Prelude.||" = return []
 | n == name "Prelude.not" = return []
 | n == name "Prelude.__prim_eq_Integer" = return []
 | n == name "Prelude.__prim_eq_Char" = return []
 | n == name "Prelude.valueof" = return []
 | n == name "Prelude.numeric" = return []
 | n == name "Prelude.error" = return []
 | n == name "Seri.Bit.__prim_fromInteger_Bit" = return []
 | n == name "Seri.Bit.__prim_eq_Bit" = return []
 | n == name "Seri.Bit.__prim_add_Bit" = return []
 | n == name "Seri.Bit.__prim_sub_Bit" = return []
 | n == name "Seri.Bit.__prim_mul_Bit" = return []
 | n == name "Seri.Bit.__prim_concat_Bit" = return []
 | n == name "Seri.Bit.__prim_show_Bit" = return []
 | n == name "Seri.Bit.__prim_not_Bit" = return []
 | n == name "Seri.Bit.__prim_or_Bit" = return []
 | n == name "Seri.Bit.__prim_and_Bit" = return []
 | n == name "Seri.Bit.__prim_shl_Bit" = return []
 | n == name "Seri.Bit.__prim_lshr_Bit" = return []
 | n == name "Seri.Bit.__prim_zeroExtend_Bit" = return []
 | n == name "Seri.Bit.__prim_truncate_Bit" = return []
 | n == name "Seri.Bit.__prim_extract_Bit" = return []
 | n == name "Prelude.return_io" = return []
 | n == name "Prelude.bind_io" = return []
 | n == name "Prelude.nobind_io" = return []
 | n == name "Prelude.fail_io" = return []
 | n == name "Prelude.putChar" = return []

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
                 H.text "import Seri.HaskellF.Lib.Prelude"

      ds = surely $ (concat <$> mapM hsDec env)
  in hsHeader H.$+$ H.ppr ds H.$+$
        H.text "main :: IO ()" H.$+$
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

harrowsT :: [H.Type] -> H.Type
harrowsT = foldr1 (\a b -> H.AppT (H.AppT H.ArrowT a) b)

-- Tuple declarations renamed.
tuple :: Int -> Dec
tuple i = 
  let nm = name $ "Tuple" ++ show i ++ "__"
      vars = [NormalTV (name [c]) | c <- take i "abcdefghijklmnopqrstuvwxyz"]
  in DataD nm vars [Con nm (map tyVarType vars)]

