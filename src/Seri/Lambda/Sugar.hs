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
    ifE, lamE, appsE, unappsE,
    Stmt(..), doE,
    Clause(..), clauseE,
    trueE, falseE, boolE, listE, listP, tupE, tupP,
    letE, stringE, charE, integerE,
    Module(..), Import(..), flatten,
    ConRec(..), recordD, recordC, recordU,
    ) where

import Data.List((\\))

import Seri.Failable
import Seri.Lambda.IR
import Seri.Lambda.Prelude
import Seri.Lambda.Types
import Seri.Lambda.Utils

-- | True
trueE :: Exp
trueE = ConE (Sig "True" (ConT "Bool"))

-- | False
falseE :: Exp
falseE = ConE (Sig "False" (ConT "Bool"))

-- | Boolean expression
boolE :: Bool -> Exp
boolE True = trueE
boolE False = falseE

-- | if p then a else b
ifE :: Exp -> Exp -> Exp -> Exp
ifE p a b = CaseE p [Match (ConP (ConT "Bool") "True" []) a,
                     Match (ConP (ConT "Bool") "False" []) b]

data Stmt = 
    BindS Sig Exp   -- ^ n <- e
  | NoBindS Exp     -- ^ e
    deriving(Eq, Show)

-- | do { stmts }
-- The final statement of the 'do' must be a NoBindS.
doE :: [Stmt] -> Exp
doE [] = error $ "doE on empty list"
doE [NoBindS e] = e 
doE ((NoBindS e):stmts) =
    let rest = doE stmts
        tbind = (arrowsT [typeof e, typeof rest, typeof rest])
    in appsE [VarE (Sig ">>" tbind), e, rest]
doE ((BindS s e):stmts) =
    let f = LamE s (doE stmts)
        tbind = (arrowsT [typeof e, typeof f, outputT (typeof f)])
    in appsE [VarE (Sig ">>=" tbind), e, f]

-- | (a, b, ... )
-- There must be at least one expression given.
--
-- If exactly one expression is given, that expression is returned without
-- tupling.
tupE :: [Exp] -> Exp
tupE [] = error $ "tupE on empty list"
tupE [x] = x
tupE es@(_:_:_) =
    let n = length es
        name = "(" ++ replicate (n-1) ',' ++ ")"
        types = map typeof es
        ttype = arrowsT (types ++ [foldl AppT (ConT name) types])
    in foldl AppE (ConE (Sig name ttype)) es

-- | (a, b, ... )
-- There must be at least one pattern given.
--
-- If exactly one pattern is given, that pattern is returned without
-- tupling.
tupP :: [Pat] -> Pat
tupP [] = error $ "tupP on empty list"
tupP [p] = p
tupP ps@(_:_:_) =
    let n = length ps
        name = "(" ++ replicate (n-1) ',' ++ ")"
        types = map typeof ps
        ttype = foldl AppT (ConT name) types
    in ConP ttype name ps
    

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

      args = [[c] | c <- take nargs "abcdefghijklmnopqrstuvwxyz"]
      casearg = tupE [VarE (Sig n (typeof p)) | (n, p) <- zip args pats1]
      caseexp = CaseE casearg (map mkmatch clauses)
      lamargs = [Sig n (typeof p) | (n, p) <- zip args pats1]
      
  in lamE lamargs caseexp

-- | \a b ... c -> e
lamE :: [Sig] -> Exp -> Exp
lamE [] e = e
lamE (x:xs) e = LamE x (lamE xs e)

-- | (a b ... c)
appsE :: [Exp] -> Exp
appsE = foldl1 AppE

-- | Given (a b ... c), returns [a, b, ..., c]
unappsE :: Exp -> [Exp]
unappsE (AppE a b) = unappsE a ++ [b]
unappsE e = [e]

-- | [a, b, ..., c]
listE :: [Exp] -> Exp
listE [] = ConE (Sig "[]" (listT UnknownT))
listE [x] =
 let t = typeof x
     consT = arrowsT [t, listT t, listT t]
 in appsE [ConE (Sig ":" consT), x, ConE (Sig "[]" (listT t))]
listE (x:xs) = 
 let t = typeof x
     consT = arrowsT [t, listT t, listT t]
 in appsE [ConE (Sig ":" consT), x, listE xs]

listP :: [Pat] -> Pat
listP [] = ConP (listT UnknownT) "[]" []
listP [x] =
  let t = listT $ typeof x
  in ConP t ":" [x, ConP t "[]" []]
listP (x:xs) =
  let t = listT $ typeof x
  in ConP t ":" [x, listP xs]

-- |
-- > let n1 = e1
-- >     n2 = e2
-- >     ...
-- > in e
-- Recursive bindings are not allowed.
letE :: [(Sig, Exp)] -> Exp -> Failable Exp
letE [] x = return x
letE ((Sig n t, v):bs) x =
  let tobind = n : map ((\(Sig n _) -> n) . fst) bs
      recursive = filter (\(Sig v _) -> v `elem` tobind) (free v)
  in if null recursive
        then do
            sub <- letE bs x
            return (AppE (LamE (Sig n t) sub) v)
        else fail $ "let expression is recursive in vars: " ++ show recursive

integerE :: Integer -> Exp
integerE i = LitE (IntegerL i)

charE :: Char -> Exp
charE c = LitE (CharL c)

stringE :: [Char] -> Exp
stringE [] = ConE (Sig "[]" (listT charT))
stringE s = listE (map charE s)

-- | Currently imports are restricted to the form:
-- > import Foo.Bar
-- No hiding or qualification is supported.
data Import = Import Name
    deriving(Show, Eq)

data Module = Module Name [Import] [Dec]
    deriving(Show, Eq)

-- | Flatten a module hierarchy.
-- This doesn't currently do much, but eventually it's expected it will
-- implement name resolution and qualification of identifiers.
flatten :: [Module] -> [Dec]
flatten ms = prelude ++ concat [d | Module _ _ d <- ms]


-- | Record type constructors.
data ConRec = NormalC Name [Type]
            | RecordC Name [(Name, Type)]
    deriving(Eq, Show)


-- return the undef variable name for a given data constructor name.
record_undefnm :: Name -> Name
record_undefnm n = "__" ++ n ++ "_undef"

-- return the updater for a given field.
record_updnm :: Name -> Name
record_updnm n = "__" ++ n ++ "_update"

-- | Desugar record constructors from a data declaration.
-- Generates:
--   data declaration with normal constructors.
--   accessor functions for every field.
--   update functions for every field.
--   An undef declaration for each constructor.
recordD :: Name -> [Name] -> [ConRec] -> [Dec]
recordD nm vars cons =
  let mkcon :: ConRec -> Con
      mkcon (NormalC n ts) = Con n ts
      mkcon (RecordC n ts) = Con n (map snd ts)

      dt = appsT (ConT nm : map VarT vars)

      mkundef :: Con -> Dec
      mkundef (Con n ts) =
        let undefnm = (record_undefnm n)
            undefet = arrowsT $ ts ++ [dt]
            undefe = appsE $ ConE (Sig n undefet) : [VarE (Sig "undefined" t) | t <- ts]
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
                         ++ [VarP (Sig "x" t)]
                         ++ [WildP pt | (_, pt) <- drop (i+1) ts])
                  body = clauseE [Clause [pat] (VarE (Sig "x" t))]
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
  in concat [[DataD nm vars cons'], undefs, accs, upds]

-- | Desugar labelled update.
recordU :: Exp -> [(Name, Exp)] -> Exp
recordU e [] = e
recordU e ((n, v):us) = 
  appsE [VarE (Sig (record_updnm n) (arrowsT [typeof v, typeof e])),
         v, recordU e us]

-- | Desugar labelled constructors.
recordC :: Sig -> [(Name, Exp)] -> Exp
recordC (Sig cn ct) fields = recordU (VarE (Sig (record_undefnm cn) ct)) fields

