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

-- | The builtin seri prelude and haskell functions for building up 
-- seri expressions based on the prelude.
module Seri.Lambda.Prelude (
    prelude,
    appsE, unappsE, 
    trueE, falseE, boolE, listE, listP, tupE, tupP,
    stringE, charE, integerE, numberE, bitE,
    ) where

import Seri.Lambda.IR
import Seri.Lambda.Types

tuple :: Int -> Dec
tuple i = 
  let nm = name $ "(" ++ replicate (i-1) ',' ++ ")"
      vars = [NormalTV (name [c]) | c <- take i "abcdefghijklmnopqrstuvwxyz"]
  in DataD nm vars [Con nm (map tyVarType vars)]

prelude :: [Dec]
prelude = [
    DataD (name "Char") [] [],
    DataD (name "Integer") [] [],
    DataD (name "()") [] [Con (name "()") []],
    tuple 2, tuple 3, tuple 4,
    DataD (name "[]") [NormalTV (name "a")] [Con (name "[]") [], Con (name ":") [VarT (name "a"), listT (VarT (name "a"))]]
    ]

-- | True
trueE :: Exp
trueE = ConE (Sig (name "True") boolT)

-- | False
falseE :: Exp
falseE = ConE (Sig (name "False") boolT)

-- | Boolean expression
boolE :: Bool -> Exp
boolE True = trueE
boolE False = falseE

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
      nm = name $ "(" ++ replicate (n-1) ',' ++ ")"
      types = map typeof es
      ttype = arrowsT (types ++ [tupT types])
  in foldl AppE (ConE (Sig nm ttype)) es

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
        nm = name $ "(" ++ replicate (n-1) ',' ++ ")"
        types = map typeof ps
        ttype = foldl AppT (ConT nm) types
    in ConP ttype nm ps
    
-- | [a, b, ..., c]
listE :: [Exp] -> Exp
listE [] = ConE (Sig (name "[]") (listT UnknownT))
listE [x] =
 let t = typeof x
     consT = arrowsT [t, listT t, listT t]
 in appsE [ConE (Sig (name ":") consT), x, ConE (Sig (name "[]") (listT t))]
listE (x:xs) = 
 let t = typeof x
     consT = arrowsT [t, listT t, listT t]
 in appsE [ConE (Sig (name ":") consT), x, listE xs]

listP :: [Pat] -> Pat
listP [] = ConP (listT UnknownT) (name "[]") []
listP [x] =
  let t = listT $ typeof x
  in ConP t (name ":") [x, ConP t (name "[]") []]
listP (x:xs) =
  let t = listT $ typeof x
  in ConP t (name ":") [x, listP xs]

integerE :: Integer -> Exp
integerE i = LitE (IntegerL i)

numberE :: Integer -> Exp
numberE i = AppE (VarE (Sig (name "fromInteger") (arrowsT [integerT, UnknownT]))) (integerE i)

bitE :: Integer -> Integer -> Exp
bitE w v = AppE (VarE (Sig (name "Seri.Lib.Bit.__prim_fromInteger_Bit") (arrowsT [integerT, AppT (ConT (name "Bit")) (NumT (ConNT w))]))) (integerE v)

charE :: Char -> Exp
charE c = LitE (CharL c)

stringE :: [Char] -> Exp
stringE [] = ConE (Sig (name "[]") (listT charT))
stringE s = listE (map charE s)

-- | (a b ... c)
appsE :: [Exp] -> Exp
appsE = foldl1 AppE

-- | Given (a b ... c), returns [a, b, ..., c]
unappsE :: Exp -> [Exp]
unappsE (AppE a b) = unappsE a ++ [b]
unappsE e = [e]

