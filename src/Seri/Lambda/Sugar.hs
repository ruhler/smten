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

-- | Constructor functions for desugaring higher level constructs into the
-- core Seri IR.
module Seri.Lambda.Sugar (
    ifE, lamE, letE,
    Stmt(..), doE,
    Clause(..), clauseE,
    ConRec(..), recordD, recordC, recordU,
    --deriveEq,
    ) where

import Data.List((\\))

import Seri.Failable
import Seri.Lambda.IR
import Seri.Lambda.Prelude
import Seri.Lambda.Types
import Seri.Lambda.Utils

-- | if p then a else b
ifE :: Exp -> Exp -> Exp -> Exp
ifE p a b = CaseE p [Match (ConP (ConT (name "Bool")) (name "True") []) a,
                     Match (ConP (ConT (name "Bool")) (name "False") []) b]

-- | \a b ... c -> e
lamE :: [Pat] -> Exp -> Exp
lamE [] e = e
lamE (VarP x : xs) e = LamE x (lamE xs e)
lamE (p : xs) e =
 let -- TODO: I hope this doesn't shadow anyone's variable use...
     x = Sig (name "__e") (typeof p)
 in LamE x (CaseE (VarE x) [Match p (lamE xs e)])

-- |
-- > let n1 = e1
-- >     n2 = e2
-- >     ...
-- > in e
letE :: [(Pat, Exp)] -> Exp -> Exp
letE [] x = x
letE ((VarP (Sig n t), v):bs) x = AppE (LamE (Sig n t) (letE bs x)) v
letE ((p, e):bs) x =
  let -- Here we implement lazy pattern matching.
      -- for example, given:
      --    let (a, b) = foo bar in ...
      --
      -- We desugar this into:
      --    let __e = foo bar
      --        a = case __e of
      --               (a, _) -> a
      --        b = case __e of
      --               (_, b) -> b
      --
      -- The reason for introducing the variable __e is to avoid duplicating
      -- the expression, which could be complex.
      --
      -- The reason we replace unused variable patterns with wildcard patterns
      -- is to avoid ambiguous types (is this really an issue though?) and
      -- because it is better code? Not sure totally if this is worth it.

      devpat :: Sig -> Pat -> Pat
      devpat keep =
        let dp :: Pat -> Pat
            dp (ConP t n ps) = ConP t n (map dp ps)
            dp p@(VarP s) | s == keep = p
            dp p@(VarP (Sig _ t)) = WildP t
            dp p@(LitP {}) = p
            dp p@(WildP {}) = p
        in dp
      
      vars = bindingsP p

      -- TODO: I hope we aren't shadowing someone's use of the variable "__e".
      evar = Sig (name "__e") (typeof e)
      vals = [(VarP v, CaseE (VarE evar) [Match (devpat v p) (VarE v)]) | v <- vars]
  in letE ((VarP evar, e):vals) (letE bs x)

data Stmt = 
    BindS Pat Exp   -- ^ n <- e
  | NoBindS Exp     -- ^ e
  | LetS Pat Exp    -- ^ let p = e
    deriving(Eq, Show)

-- | do { stmts }
-- The final statement of the 'do' must be a NoBindS.
doE :: [Stmt] -> Exp
doE [] = error $ "doE on empty list"
doE [NoBindS e] = e 
doE ((LetS p e):stmts) =
    let rest = doE stmts
    in letE [(p, e)] rest
doE ((NoBindS e):stmts) =
    let rest = doE stmts
        tbind = (arrowsT [typeof e, typeof rest, typeof rest])
    in appsE [VarE (Sig (name ">>") tbind), e, rest]
doE ((BindS p e):stmts) =
    let f = lamE [p] (doE stmts)
        tbind = (arrowsT [typeof e, typeof f, outputT (typeof f)])
    in appsE [VarE (Sig (name ">>=") tbind), e, f]

data Clause = Clause [Pat] Exp
    
-- | Given a set of function clauses, return a corresponding expression to
-- implement those clauses.
--
-- All clauses must have the same number of patterns. The number of patterns
-- may be 0. At least one clause must be given.
clauseE :: [Clause] -> Exp
clauseE [] = error $ "clauseE on empty list"
clauseE ((Clause [] e):_) = e
clauseE clauses@(_:_) = 
  let Clause pats1 _ = head clauses
      nargs = length pats1
    
      mkmatch :: Clause -> Match
      mkmatch (Clause pats body) = Match (tupP pats) body

      args = [name [c] | c <- take nargs "abcdefghijklmnopqrstuvwxyz"]
      casearg = tupE [VarE (Sig n (typeof p)) | (n, p) <- zip args pats1]
      caseexp = CaseE casearg (map mkmatch clauses)
      lamargs = [VarP (Sig n (typeof p)) | (n, p) <- zip args pats1]
      
  in lamE lamargs caseexp

-- | Record type constructors.
data ConRec = NormalC Name [Type]
            | RecordC Name [(Name, Type)]
    deriving(Eq, Show)


-- return the undef variable name for a given data constructor name.
record_undefnm :: Name -> Name
record_undefnm n = name "__" `nappend` n `nappend` name "_undef"

-- return the updater for a given field.
record_updnm :: Name -> Name
record_updnm n = name "__" `nappend` n `nappend` name "_update"

-- | Desugar record constructors from a data declaration.
-- Also handles deriving construts.
-- Generates:
--   data declaration with normal constructors.
--   accessor functions for every field.
--   update functions for every field.
--   An undef declaration for each constructor.
recordD :: Name -> [TyVar] -> [ConRec] -> [String] -> [Dec]
recordD nm vars cons derivings =
  let mkcon :: ConRec -> Con
      mkcon (NormalC n ts) = Con n ts
      mkcon (RecordC n ts) = Con n (map snd ts)

      dt = appsT (ConT nm : map tyVarType vars)

      mkundef :: Con -> Dec
      mkundef (Con n ts) =
        let undefnm = (record_undefnm n)
            undefet = arrowsT $ ts ++ [dt]
            undefe = appsE $ ConE (Sig n undefet) : [VarE (Sig (name "undefined") t) | t <- ts]
        in ValD (TopSig undefnm [] dt) undefe

      -- TODO: handle correctly the case where two different constructors
      -- share the same accessor name.
      mkaccs :: ConRec -> [Dec]
      mkaccs (NormalC {}) = []
      mkaccs (RecordC cn ts) = 
        let mkacc :: ((Name, Type), Int) -> Dec
            mkacc ((n, t), i) = 
              let at = arrowsT [dt, t] 
                  pat = ConP dt cn ([WildP pt | (_, pt) <- take i ts]
                         ++ [VarP (Sig (name "x") t)]
                         ++ [WildP pt | (_, pt) <- drop (i+1) ts])
                  body = clauseE [Clause [pat] (VarE (Sig (name "x") t))]
              in ValD (TopSig n [] at) body
        in map mkacc (zip ts [0..])

      mkupds :: ConRec -> [Dec]
      mkupds (NormalC {}) = []
      mkupds (RecordC cn ts) = 
        let ct = arrowsT $ (map snd ts) ++ [dt]
            mkupd :: ((Name, Type), Int) -> Dec
            mkupd ((n, t), i) =
              let ut = arrowsT [t, dt, dt]
                  mypat = ConP dt cn (
                            [VarP (Sig nm t) | (nm, t) <- take i ts]
                            ++ [WildP t]
                            ++ [VarP (Sig nm t) | (nm, t) <- drop (i+1) ts])
                  myexp = appsE $ ConE (Sig cn ct) : [VarE (Sig n t) | (n, t) <- ts]
                  body = clauseE [Clause [VarP (Sig n t), mypat] myexp]
              in ValD (TopSig (record_updnm n) [] ut) body
        in map mkupd (zip ts [0..])
                            
      cons' = map mkcon cons
      undefs = map mkundef cons'
      accs = concatMap mkaccs cons
      upds = concatMap mkupds cons
      derivations = [derive d nm vars cons' | d <- derivings]
  in concat [[DataD nm vars cons'], derivations, undefs, accs, upds]

-- | Desugar labelled update.
recordU :: Exp -> [(Name, Exp)] -> Exp
recordU e [] = e
recordU e ((n, v):us) = 
  appsE [VarE (Sig (record_updnm n) (arrowsT [typeof v, typeof e])),
         v, recordU e us]

-- | Desugar labelled constructors.
recordC :: Sig -> [(Name, Exp)] -> Exp
recordC (Sig cn ct) fields = recordU (VarE (Sig (record_undefnm cn) ct)) fields

-- Derive an instance of Eq (before flattening and inference) for the given
-- data type declaration.
deriveEq :: Name -> [TyVar] -> [Con] -> Dec
deriveEq dn vars cs = 
  let dt = appsT (ConT dn : map tyVarType vars)
      mkcon :: Con -> Clause
      mkcon (Con cn ts) = 
        let fields1 = [Sig (name $ [c, '1']) t | (t, c) <- zip ts "abcdefghijklmnopqrstuvwxyz"]
            fields2 = [Sig (name $ [c, '2']) t | (t, c) <- zip ts "abcdefghijklmnopqrstuvwxyz"]
            p1 = ConP dt cn [VarP s | s <- fields1]
            p2 = ConP dt cn [VarP s | s <- fields2]
            body = AppE (VarE (Sig (name "and") UnknownT))
                        (listE [appsE [VarE (Sig (name "==") UnknownT), VarE a, VarE b] | (a, b) <- zip fields1 fields2])
        in Clause [p1, p2] body

      def = Clause [WildP UnknownT, WildP UnknownT] falseE
      ctx = [Class (name "Eq") [tyVarType c] | c <- vars]
      eqclauses = map mkcon cs ++ [def]
      eq = Method (name "==") (clauseE eqclauses)
      ne = Method (name "/=") (VarE (Sig (name "/=#") UnknownT))
  in InstD ctx (Class (name "Eq") [dt]) [eq, ne]

derive :: String -> Name -> [TyVar] -> [Con] -> Dec
derive "Eq" = deriveEq
derive x = error $ "deriving " ++ show x ++ " not supported in seri"

