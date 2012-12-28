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
import Data.List(nub, genericLength)
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
hsName n
 | Just i <- de_tupleN n = H.mkName $ "Tuple" ++ show i ++ "__"
 | n == name "()" = H.mkName $ "Unit__"
 | n == name ":" = H.mkName $ "Cons__"
 | n == name "[]" = H.mkName $ "Nil__"
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


hsLit :: Lit -> H.Exp
hsLit l
 | Just i <- de_integerL l = H.LitE (H.IntegerL i)
 | Just c <- de_charL l = H.AppE (H.VarE (H.mkName "S.seriS")) (H.LitE (H.CharL c))

prependnm :: String -> Name -> H.Name
prependnm m n = hsName $ name m `nappend` n

-- Given the name of a data constructor, return the name of the function for
-- doing a case match against the constructor.
constrcasenm :: Name -> H.Name
constrcasenm n 
 | n == name "()" = constrcasenm $ name "Unit__"
 | Just x <- de_tupleN n = constrcasenm . name $ "Tuple" ++ show x ++ "__"
 | n == name "[]" = constrcasenm $ name "Nil__"
 | n == name ":" = constrcasenm $ name "Cons__"
constrcasenm n = prependnm "__case" n

symconstrnm :: Name -> H.Name
symconstrnm n
 | Just x <- de_tupleN n = symconstrnm . name $ "Tuple" ++ show x ++ "__"
 | otherwise = hsName $ n `nappend` (name "__s")

hsExp :: Exp -> Failable H.Exp

-- String literals:
-- TODO: the template haskell pretty printer doesn't print strings correctly
-- if they contain newlines, thus, we can't print those as string literals.
-- When they fix the template haskell pretty printer, that special case should
-- be removed here.
hsExp e
  | Just str <- de_stringE e
  , '\n' `notElem` str
    = return $ H.AppE (H.VarE (H.mkName "S.seriS")) (H.LitE (H.StringL str))

hsExp (LitE l) = return (hsLit l)
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

-- case x of
--    K -> y
--    _ -> n
--
-- Translates to:  __caseK x y n
hsExp (CaseE x (Sig kn kt) y n) = do
    [x', y', n'] <- mapM hsExp [x, y, n]
    return $ foldl1 H.AppE [H.VarE (constrcasenm kn), x', y', n']
        
hsType :: Type -> Failable H.Type
hsType (ConT n)
  | n == name "()" = return $ H.ConT (H.mkName "Unit__")
  | Just x <- de_tupleN n
     = return $ H.ConT (H.mkName $ "Tuple" ++ show x ++ "__")
  | n == name "[]" = return $ H.ConT (H.mkName "List__")
  | n == name "->" = return H.ArrowT
  | otherwise = return $ H.ConT (hsName n)
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
hsType t = throw $ "haskellf: unsupported type: " ++ pretty t

-- Return the numeric type corresponding to the given integer.
hsnt :: Integer -> H.Type
hsnt 0 = H.ConT (H.mkName "N__0")
hsnt n = H.AppT (H.ConT (H.mkName $ "N__2p" ++ show (n `mod` 2))) (hsnt $ n `div` 2)

hsTopType :: [Name] -> Context -> Type -> Failable H.Type
hsTopType clsvars ctx t = do
    let (nctx, use) = mkContext (flip notElem clsvars) t
    t' <- hsType t
    ctx' <- mapM hsClass ctx
    case nctx ++ ctx' of
        [] -> return t'
        ctx'' -> return $ H.ForallT (map (H.PlainTV . hsName) use) ctx'' t'

hsClass :: Class -> Failable H.Pred
hsClass (Class nm ts) = do
    ts' <- mapM hsType ts
    return $ H.ClassP (hsName nm) ts'
    
hsMethod :: Method -> Failable H.Dec
hsMethod (Method n e) = do
    let hsn = hsName n
    e' <- hsExp e
    return $ H.ValD (H.VarP hsn) (H.NormalB e') []


hsSig :: [Name]     -- ^ List of varTs to ignore, because they belong to the class.
         -> TopSig
         -> Failable H.Dec
hsSig clsvars (TopSig n ctx t) = do
    t' <- hsTopType clsvars ctx t
    return $ H.SigD (hsName n) t'

    
hsDec :: Dec -> Failable [H.Dec]
hsDec (ValD (TopSig n ctx t) e) = do
    t' <- hsTopType [] ctx t
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
  name "Answer",
  name "Query",
  name "IO"] = return []

hsDec (DataD n tyvars constrs) = do
    dataD <- mkDataD n tyvars constrs
    seriTD <- mkSeriTD n tyvars
    symbD <- mkSymbD n tyvars constrs
    casesD <- mapM (mkCaseD n tyvars) constrs
    return $ concat ([dataD, seriTD, symbD] : casesD)

hsDec (ClassD n vars sigs@(TopSig _ _ t:_)) = do
    let vts = map tyVarName vars
        (ctx, use) = mkContext (flip elem vts) t
    sigs' <- mapM (hsSig vts) sigs
    return $ [H.ClassD ctx (hsName n) (map (H.PlainTV . hsName) use) [] sigs']

hsDec (InstD ctx (Class n ts) ms) = do
    let (nctx, _) = mkContext (const True) (appsT (conT n) ts)
    ctx' <- mapM hsClass ctx
    ms' <- mapM hsMethod ms
    ts' <- mapM hsType ts
    let t = foldl H.AppT (H.ConT (hsName n)) ts'
    return [H.InstanceD (nctx ++ ctx') t ms'] 

hsDec (PrimD s@(TopSig n _ _))
 | n == name "Prelude.__prim_add_Integer" = return []
 | n == name "Prelude.__prim_sub_Integer" = return []
 | n == name "Prelude.__prim_mul_Integer" = return []
 | n == name "Prelude.__prim_show_Integer" = return []
 | n == name "Prelude.__prim_lt_Integer" = return []
 | n == name "Prelude.__prim_leq_Integer" = return []
 | n == name "Prelude.__prim_gt_Integer" = return []
 | n == name "Prelude.__prim_geq_Integer" = return []
 | n == name "Prelude.__prim_eq_Integer" = return []
 | n == name "Prelude.__prim_toInteger_Char" = return []
 | n == name "Prelude.__prim_fromInteger_Char" = return []
 | n == name "Prelude.valueof" = return []
 | n == name "Prelude.numeric" = return []
 | n == name "Prelude.error" = return []
 | n == name "Seri.Bit.__prim_fromInteger_Bit" = return []
 | n == name "Seri.Bit.__prim_eq_Bit" = return []
 | n == name "Seri.Bit.__prim_lt_Bit" = return []
 | n == name "Seri.Bit.__prim_gt_Bit" = return []
 | n == name "Seri.Bit.__prim_leq_Bit" = return []
 | n == name "Seri.Bit.__prim_geq_Bit" = return []
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
 | n == name "Prelude.getContents" = return []
 | n == name "Debug.Trace.trace" = return []
 | n == name "Seri.SMT.SMT.__prim_free" = return []
 | n == name "Seri.SMT.SMT.assert" = return []
 | n == name "Seri.SMT.SMT.query" = return []
 | n == name "Seri.SMT.SMT.queryS" = return []
 | n == name "Seri.SMT.SMT.return_query" = return []
 | n == name "Seri.SMT.SMT.nobind_query" = return []
 | n == name "Seri.SMT.SMT.bind_query" = return []
 | n == name "Seri.SMT.SMT.fail_query" = return []
 | n == name "Seri.SMT.SMT.runYices1" = return []
 | n == name "Seri.SMT.SMT.runYices2" = return []
 | n == name "Seri.SMT.SMT.runSTP" = return []

hsDec d = throw $ "coreH does not apply to dec: " ++ pretty d

-- haskell decs
--  Compile the given declarations to haskell.
haskellf ::    Bool     -- ^ Should a "__main" wrapper be generated?
            -> String   -- ^ Name of target module.
            -> [Dec] -> H.Doc
haskellf wrapmain modname env =
  let hsHeader :: H.Doc
      hsHeader = H.text "{-# LANGUAGE ExplicitForAll #-}" H.$+$
                 H.text "{-# LANGUAGE MultiParamTypeClasses #-}" H.$+$
                 H.text "{-# LANGUAGE FlexibleInstances #-}" H.$+$
                 H.text "{-# LANGUAGE ScopedTypeVariables #-}" H.$+$
                 H.text ("module " ++ modname ++ " where") H.$+$
                 H.text "import qualified Prelude" H.$+$
                 H.text "import qualified Seri.HaskellF.Symbolic as S" H.$+$
                 H.text "import qualified Seri.Name as S" H.$+$
                 H.text "import qualified Seri.Type as S" H.$+$
                 H.text "import qualified Seri.ExpH as S" H.$+$
                 H.text "import Seri.HaskellF.Lib.Prelude" H.$+$
                 H.text "import Seri.HaskellF.Lib.SMT" H.$+$
                 H.text "" H.$+$
                 if wrapmain
                    then H.text "__main = __main_wrapper main"
                    else H.empty

      ds = surely $ (concat <$> mapM hsDec env)
  in hsHeader H.$+$ H.ppr ds

unknowntype :: Type -> Bool
unknowntype (ConT {}) = False
unknowntype (AppT a b) = unknowntype a || unknowntype b
unknowntype (VarT {}) = True
unknowntype (NumT (VarNT {})) = True
unknowntype (NumT {}) = False
unknowntype UnknownT = True

harrowsT :: [H.Type] -> H.Type
harrowsT = foldr1 (\a b -> H.AppT (H.AppT H.ArrowT a) b)

clssymbolic :: Integer -> H.Name
clssymbolic 0 = H.mkName "S.Symbolic"
clssymbolic n = H.mkName $ "S.Symbolic" ++ show n

boxmeth :: Integer -> H.Name
boxmeth 0 = H.mkName "box"
boxmeth n = H.mkName $ "box" ++ show n

unboxmeth :: Integer -> H.Name
unboxmeth 0 = H.mkName "unbox"
unboxmeth n = H.mkName $ "unbox" ++ show n

clsserit :: Integer -> H.Name
clsserit 0 = H.mkName "S.SeriT"
clsserit n = H.mkName $ "S.SeriT" ++ show n

seritmeth :: Integer -> H.Name
seritmeth 0 = H.mkName "seriT"
seritmeth n = H.mkName $ "seriT" ++ show n

-- Form the context for declarations.
mkContext :: (Name -> Bool) -- ^ which variable types we should care about
              -> Type       -- ^ a sample use of the variable types
              -> ([H.Pred], [Name])  -- ^ generated context and list of names used.
mkContext p t =
  let nvts = filter p $ nvarTs t
      kvts = filter (p . fst) $ kvarTs t
      ntvs = [H.ClassP (clssymbolic 0) [H.VarT (hsName n)] | n <- nvts]
      stvs = [H.ClassP (clssymbolic k) [H.VarT (hsName n)] | (n, k) <- kvts]
  in (concat [ntvs, stvs], nvts ++ map fst kvts)

hsCon :: Con -> Failable H.Con
hsCon (Con n tys) = do
    ts <- mapM hsType tys
    return $ H.NormalC (hsName n) (map (\t -> (H.NotStrict, t)) ts)

-- data Foo a b ... =
--    FooA FooA1 FooA2 ...
--  | FooB FooB1 FooB2 ...
--    ...
--  | Foo__s ExpH
mkDataD :: Name -> [TyVar] -> [Con] -> Failable H.Dec
mkDataD n tyvars constrs = do
  let tyvars' = map (H.PlainTV . hsName . tyVarName) tyvars
  constrs' <- mapM hsCon constrs
  let sconstr = H.NormalC (symconstrnm n) [(H.NotStrict, H.ConT (H.mkName "S.ExpH"))]
  return $ H.DataD [] (hsName n) tyvars' (constrs' ++ [sconstr]) []

-- instance SeriTN Foo where
--    seriTN _ = conT "Foo"
mkSeriTD :: Name -> [TyVar] -> Failable H.Dec
mkSeriTD n tyvars = return $
  let body = H.AppE (H.VarE (H.mkName "S.conT"))
                    (H.AppE (H.VarE (H.mkName "S.name"))
                            (H.LitE (H.StringL (unname n))))
      serit = H.FunD (seritmeth (genericLength tyvars)) [
                H.Clause [H.WildP] (H.NormalB body) []]
      clsnamet = clsserit (genericLength tyvars)
      tyt = H.AppT (H.ConT clsnamet) (H.ConT (hsName n))
  in H.InstanceD [] tyt [serit]
  
-- instance S.SymbolicN Foo where
--  boxN ...
--  unboxN ...
mkSymbD :: Name -> [TyVar] -> [Con] -> Failable H.Dec
mkSymbD n tyvars constrs = do
    boxD <- mkBoxD n tyvars constrs
    unboxD <- mkUnboxD n tyvars constrs
    let clsname = clssymbolic (genericLength tyvars)
        ty = H.AppT (H.ConT clsname) (H.ConT (hsName n))
    return $ H.InstanceD [] ty [boxD, unboxD]

--  boxN e
--   | Just [a, b, ...] <- de_conS "FooA" e = FooA (box a) (box b) ...
--   | Just [a, b, ...] <- de_conS "FooB" e = FooB (box a) (box b) ...
--   ...
--   | otherwise = Foo__s e
mkBoxD :: Name -> [TyVar] -> [Con] -> Failable H.Dec
mkBoxD n tyvars constrs = do
  let boxnm = boxmeth (genericLength tyvars)
      
      mkGuard :: Con -> (H.Guard, H.Exp)
      mkGuard (Con cn tys) = 
        let argnms = [H.mkName ("x" ++ show i) | i <- [1..length tys]]
            pat = H.ConP (H.mkName "Prelude.Just") [H.ListP (map H.VarP argnms)]
            src = foldl1 H.AppE [
                    H.VarE (H.mkName "S.de_conS"),
                    H.LitE (H.StringL (unname cn)),
                    H.VarE (H.mkName "e")]
            guard = H.PatG [H.BindS pat src]
            boxes = [H.AppE (H.VarE (H.mkName "S.box")) (H.VarE an) | an <- argnms]
            body = foldl H.AppE (H.ConE (hsName cn)) boxes
        in (guard, body)

      sguard = (H.NormalG (H.VarE (H.mkName "Prelude.otherwise")), H.AppE (H.ConE (symconstrnm n)) (H.VarE (H.mkName "e")))
      guards = map mkGuard constrs ++ [sguard]
      clause = H.Clause [H.VarP (H.mkName "e")] (H.GuardedB guards) []
  return $ H.FunD boxnm [clause]
  

--  unboxN x
--   | FooA a b ... <- x = conS x "FooA" [unbox a, unbox b, ...]
--   | FooB a b ... <- x = conS x "FooB" [unbox a, unbox b, ...]
--   ...
--   | Foo__s v <- x = v
mkUnboxD :: Name -> [TyVar] -> [Con] -> Failable H.Dec
mkUnboxD n tyvars constrs = do
    let unboxnm = unboxmeth (genericLength tyvars)

        mkGuard :: Con -> (H.Guard, H.Exp)
        mkGuard (Con cn tys) =
          let argnms = [H.mkName ("x" ++ show i) | i <- [1..length tys]]
              pat = H.ConP (hsName cn) (map H.VarP argnms)
              src = H.VarE (H.mkName "x")
              guard = H.PatG [H.BindS pat src]
              unboxes = [H.AppE (H.VarE (H.mkName "S.unbox")) (H.VarE an) | an <- argnms]
              body = foldl1 H.AppE [
                        H.VarE (H.mkName "S.conS"),
                        H.VarE (H.mkName "x"),
                        H.LitE (H.StringL (unname cn)),
                        H.ListE unboxes]
          in (guard, body)

        spat = H.ConP (symconstrnm n) [H.VarP (H.mkName "v")]
        ssrc = H.VarE (H.mkName "x")
        sguard = (H.PatG [H.BindS spat ssrc], H.VarE (H.mkName "v"))
        guards = map mkGuard constrs ++ [sguard]
        clause = H.Clause [H.VarP (H.mkName "x")] (H.GuardedB guards) []
    return $ H.FunD unboxnm [clause]

-- __caseFooB :: Foo -> (FooB1 -> FooB2 -> ... -> z) -> z -> z
-- __caseFooB x y n
--   | FooB a b ... <- x = y a b ...
--   | Foo__s _ <- x = caseS "FooB" x y n
--   | otherwise = n
mkCaseD :: Name -> [TyVar] -> Con -> Failable [H.Dec]
mkCaseD n tyvars (Con cn tys) = do
  let dt = appsT (conT n) (map tyVarType tyvars)
      z = VarT (name "z")
      t = arrowsT [dt, arrowsT (tys ++ [z]), z, z]
  ht <- hsTopType [] [] t
  let sigD = H.SigD (constrcasenm cn) ht

      body = H.AppE (H.VarE (H.mkName "S.caseS")) (H.LitE (H.StringL (unname cn)))

      yargs = [H.mkName ("x" ++ show i) | i <- [1..length tys]]
      ypat = H.ConP (hsName cn) (map H.VarP yargs)
      ysrc = H.VarE (H.mkName "x")
      ybody = foldl H.AppE (H.VarE (H.mkName "y")) [H.VarE an | an <- yargs]
      yguard = (H.PatG [H.BindS ypat ysrc], ybody)

      spat = H.ConP (symconstrnm n) [H.WildP]
      ssrc = H.VarE (H.mkName "x")
      sbody = foldl1 H.AppE [
                H.VarE (H.mkName "S.caseS"),
                H.LitE (H.StringL (unname cn)),
                H.VarE (H.mkName "x"),
                H.VarE (H.mkName "y"),
                H.VarE (H.mkName "n")]
      sguard = (H.PatG [H.BindS spat ssrc], sbody)

      nguard = (H.NormalG (H.VarE (H.mkName "Prelude.otherwise")), H.VarE (H.mkName "n"))

      guards = [yguard, sguard, nguard]
      clause = H.Clause [H.VarP (H.mkName [c]) | c <- "xyn"] (H.GuardedB guards) []
      funD = H.FunD (constrcasenm cn) [clause]
  return [sigD, funD]


