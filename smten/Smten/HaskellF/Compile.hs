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
import Smten.HaskellF.Compile.Type
import Smten.HaskellF.Compile.Exp
import Smten.HaskellF.Compile.Data

hsMethod :: Class -> Method -> HF [H.Dec]
hsMethod cls (Method n e) = do
    env <- asks hfs_env
    mt <- lookupMethodType env n cls
    mctx <- lookupMethodContext env n cls
    hsTopExp (TopExp (TopSig n mctx mt) e)


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

