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

{-# LANGUAGE FlexibleInstances #-}

-- | Pretty printer for core seri language.
module Seri.Lambda.Ppr (
    Ppr(..), pretty,
    module Text.PrettyPrint.HughesPJ,
    tabwidth,
    ) where

import Debug.Trace

import Data.List(group, nub)

import Text.PrettyPrint.HughesPJ

import Seri.Lambda.IR
import Seri.Lambda.Sugar
import Seri.Lambda.Prelude

class Ppr a where
    ppr :: a -> Doc

tabwidth :: Int
tabwidth = 2

isAtomT :: Type -> Bool
isAtomT (AppT (ConT n) _) | n == name "[]" = True
isAtomT (AppT (AppT (ConT n) a) b) | n == name "(,)" = True
isAtomT (AppT (AppT (AppT (ConT n) a) b) c) | n == name "(,,)" = True
isAtomT (ConT {}) = True
isAtomT (VarT {}) = True
isAtomT (AppT {}) = False
isAtomT (NumT {}) = True
isAtomT (UnknownT {}) = True

isArrowsT :: Type -> Bool
isArrowsT (AppT (AppT (ConT n) _) _) | n == name "->" = True
isArrowsT _ = False

instance Ppr Name where
    ppr = text . unname

instance Ppr NType where
    ppr (ConNT i) = integer i
    ppr (VarNT v) = ppr v
    ppr (AppNT o a b) = parens (ppr a <+> text o <+> ppr b)

instance Ppr Type where
    -- Special case for list
    ppr (AppT (ConT n) t) | n == name "[]" = text "[" <> ppr t <> text "]"
    
    -- Special case for tuples
    ppr (AppT (AppT (ConT n) a) b) | n == name "(,)"
        = parens . sep $ punctuate comma (map ppr [a, b])
    ppr (AppT (AppT (AppT (ConT n) a) b) c) | n == name "(,,)"
        = parens . sep $ punctuate comma (map ppr [a, b, c])
    ppr (AppT (AppT (AppT (AppT (ConT n) a) b) c) d) | n == name "(,,,)"
        = parens . sep $ punctuate comma (map ppr [a, b, c, d])

    -- Special case for ->
    ppr (AppT (AppT (ConT n) a) b) | n == name "->" && isArrowsT a
        = parens (ppr a) <+> text "->" <+> ppr b
    ppr (AppT (AppT (ConT n) a) b) | n == name "->"
        = ppr a <+> text "->" <+> ppr b
    ppr (ConT n) | n == name "->" = text "(->)"

    -- Normal cases
    ppr (ConT n) = ppr n
    ppr (VarT n) = ppr n
    ppr (AppT a b) | isAtomT b = ppr a <+> ppr b
    ppr (AppT a b) = ppr a <+> (parens $ ppr b)
    ppr (NumT n) = text "#" <> ppr n
    ppr UnknownT = text "?"


isAtomE :: Exp -> Bool
isAtomE (LitE {}) = True
isAtomE (CaseE {}) = False
isAtomE (AppE {}) = False
isAtomE (LamE {}) = False
isAtomE (ConE {}) = True
isAtomE (VarE {}) = True

isOp :: Name -> Bool
isOp n = nhead n `elem` ":!#$%&*+./<=>?@\\^|-~"

pprname :: Name -> Doc
pprname n | isOp n = parens (ppr n)
pprname n = ppr n

pprsig :: Sig -> Doc
pprsig (Sig n UnknownT) = pprname n
pprsig (Sig n t) = parens (pprname n <+> text "::" <+> (ppr t))

sep2 :: Doc -> Doc -> Doc
sep2 a b = a $$ nest tabwidth b

cando :: Exp -> Bool
cando (AppE (AppE (VarE (Sig n _)) _) _) | n == name "Seri.Lib.Prelude.>>" = True
cando (AppE (AppE (VarE (Sig n _)) _) _) | n == name ">>" = True
cando (AppE (AppE (VarE (Sig n _)) _) (LamE _ _)) | n == name "Seri.Lib.Prelude.>>=" = True
cando (AppE (AppE (VarE (Sig n _)) _) (LamE _ _)) | n == name ">>=" = True
cando _ = False

sugardo :: Exp -> [Stmt]
sugardo (AppE (AppE (VarE (Sig n _)) m) r) | n == name "Seri.Lib.Prelude.>>"
    = NoBindS m : sugardo r
sugardo (AppE (AppE (VarE (Sig n _)) m) r) | n == name ">>"
    = NoBindS m : sugardo r
sugardo (AppE (AppE (VarE (Sig n _)) m) (LamE s r)) | n == name ">>="
    = BindS (VarP s) m : sugardo r
sugardo (AppE (AppE (VarE (Sig n _)) m) (LamE s r)) | n == name "Seri.Lib.Prelude.>>="
    = BindS (VarP s) m : sugardo r
sugardo e = [NoBindS e]

isStringLiteral :: Exp -> Bool
isStringLiteral (ConE (Sig n (AppT (ConT nt) (ConT nc)))) | n == name "[]" && nt == name "[]" && nc == name "Char" = True
isStringLiteral (AppE (AppE (ConE (Sig n _)) (LitE (CharL _))) e) | n == name ":" = isStringLiteral e
isStringLiteral _ = False

stringLiteral :: Exp -> String
stringLiteral (ConE (Sig n (AppT (ConT nt) (ConT nc)))) | n == name "[]" && nt == name "[]" && nc == name "Char" = ""
stringLiteral (AppE (AppE (ConE (Sig n _)) (LitE (CharL c))) e) | n == name ":"
  = c : stringLiteral e
stringLiteral e = error $ "not a string literal: " ++ show e

isListLiteral :: Exp -> Bool
isListLiteral (ConE (Sig n (AppT (ConT nt) _))) | n == name "[]" && nt == name "[]" = True
isListLiteral (AppE (AppE (ConE (Sig n _)) _) e) | n == name ":" = isListLiteral e
isListLiteral _ = False

listLiteral :: Exp -> [Exp]
listLiteral (ConE (Sig n (AppT (ConT nt) _))) | n == name "[]" && nt == name "[]" = []
listLiteral (AppE (AppE (ConE (Sig n _)) x) e) | n == name ":" = x : listLiteral e
listLiteral e = error $ "not a list literal: " ++ show e

isLet :: Exp -> Bool
isLet (AppE (LamE {}) _) = True
isLet e = False

deLet :: Exp -> ([(Sig, Exp)], Exp)
deLet (AppE (LamE s e) v) =
  let (binds, body) = deLet e
  in ((s, v) : binds, body)
deLet e = ([], e)

pprLet :: Exp -> Doc
pprLet e = 
  let (binds, body) = deLet e
      pprbind (s, e) = pprsig s <+> text "=" <+> ppr e <+> semi
  in text "let" <+> text "{"
       $+$ nest tabwidth (vcat (map pprbind binds)) $+$ text "}"
       <+> text "in" <+> ppr body

instance Ppr Lit where
    ppr (IntegerL i) = integer i
    ppr (CharL c) = text (show c)

instance Ppr Exp where
    -- Special case for if expressions
    ppr (CaseE e [Match (ConP _ nt []) a,
                  Match (ConP _ nf []) b]) | nt == name "True" && nf == name "False"
        = text "if" <+> ppr e $$ nest tabwidth (
                text "then" <+> ppr a $$
                text "else" <+> ppr b)

    -- Special case for do statements
    ppr e | cando e
        = text "do" <+> text "{"
                $+$ nest tabwidth (vcat (map ppr (sugardo e))) $+$ text "}"

    -- Special case for tuples
    ppr e | length (untupE e) > 1 = 
        parens . sep $ punctuate comma (map ppr (untupE e))

    -- Special case for string literals
    ppr e | isStringLiteral e = text (show (stringLiteral e))

    -- Special case for list literals
    ppr e | isListLiteral e = sep $ [text "["] ++ punctuate comma (map ppr (listLiteral e)) ++ [text "]"]

    -- Special case for let expressions
    ppr e | isLet e = pprLet e

    -- Normal cases
    ppr (LitE l) = ppr l
    ppr (CaseE e ms) = text "case" <+> ppr e <+> text "of" <+> text "{"
                        $+$ nest tabwidth (vcat (map ppr ms)) $+$ text "}"
    ppr (AppE a b) | isAtomE b = ppr a <+> ppr b
    ppr (AppE a b) = ppr a <+> (parens $ ppr b)
    ppr (LamE s b) = parens $ (text "\\" <> pprsig s <+> text "->") `sep2` ppr b
    ppr (ConE s) = pprsig s
    ppr (VarE s) = pprsig s

instance Ppr Stmt where
    ppr (NoBindS e) = ppr e <> semi
    ppr (BindS p e) = ppr p <+> text "<-" <+> ppr e <> semi

instance Ppr Match where
    ppr (Match p e) = (ppr p <+> text "->") `sep2` ppr e <> semi

isAtomP :: Pat -> Bool
isAtomP (ConP _ _ []) = True
isAtomP (ConP {}) = False
isAtomP (VarP {}) = True
isAtomP (LitP {}) = True
isAtomP (WildP {}) = True

isTuple :: Name -> Bool
isTuple n = ["(", ",", ")"] == nub (group (unname n))

instance Ppr Pat where
    -- special case for tuples
    ppr (ConP _ n ps) | isTuple n = 
        parens . sep $ punctuate comma (map ppr ps)
        
    -- normal cases
    ppr (ConP t n ps) =
        let subp p | isAtomP p = ppr p
            subp p = parens (ppr p)
        in pprsig (Sig n t) <+> hsep (map subp ps)
    ppr (VarP s) = pprsig s
    ppr (LitP l) = ppr l
    ppr (WildP t) = pprsig (Sig (name "_") t)

conlist :: [Con] -> Doc
conlist [] = empty
conlist (x:xs) = text " " <+> ppr x
                    $+$ vcat (map (\c -> text "|" <+> ppr c) xs)

instance Ppr TyVar where
    ppr (NormalTV n) = ppr n
    ppr (NumericTV n) = text "#" <> ppr n

instance Ppr Dec where
    ppr (ValD s@(TopSig n _ _) e)
        = ppr s <> semi $$ pprname n <+> text "=" <+> ppr e
    ppr (DataD n vs cs)
        = text "data" <+> ppr n <+> hsep (map ppr vs) <+> text "=" $$
            (nest tabwidth (conlist cs))
    ppr (ClassD n vs ss)
        = text "class" <+> ppr n <+> hsep (map ppr vs)
                <+> text "where" <+> text "{"
                $+$ nest tabwidth (vcat (punctuate semi (map ppr ss))) $+$ text "}"
    ppr (InstD ctx cls ms)
        = text "instance"
                <+> ppr ctx
                <+> ppr cls
                <+> text "where" <+> text "{"
                $+$ nest tabwidth (vcat (map ppr ms)) $+$ text "}"
    ppr (PrimD s) = ppr s

instance Ppr Sig where
    ppr (Sig n t) = pprname n <+> text "::" <+> ppr t <> semi

instance Ppr TopSig where
    ppr (TopSig n ctx t)
      = pprname n <+> text "::" <+> ppr ctx <+> ppr t

instance Ppr Context where
    ppr [] = empty
    ppr xs = parens (sep (punctuate comma $ map ppr xs)) <+> text "=>"
    
instance Ppr Con where
    ppr (Con n ts) =
      let pprt t | isAtomT t = ppr t
          pprt t = parens (ppr t)
      in ppr n <+> hsep (map pprt ts)

instance Ppr Method where
    ppr (Method n e) = pprname n <+> text "=" <+> ppr e <> semi

instance Ppr [Dec] where
    ppr ds = vcat (punctuate semi (map (\d -> ppr d $+$ text "") ds))

instance Ppr Class where
    ppr (Class n ts) = ppr (foldl AppT (ConT n) ts)

-- | Print an object very prettily.
pretty :: (Ppr a) => a -> String
pretty = render . ppr

