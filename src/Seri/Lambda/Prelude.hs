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
    stringE, charE, integerE,
    ) where

import Seri.Lambda.IR
import Seri.Lambda.Types

tuple :: Int -> Dec
tuple i = 
  let name = "(" ++ replicate (i-1) ',' ++ ")"
      vars = [NormalTV [c] | c <- take i "abcdefghijklmnopqrstuvwxyz"]
  in DataD name vars [Con name (map tyVarType vars)]

prelude :: [Dec]
prelude = [
    DataD "Char" [] [],
    DataD "Integer" [] [],
    DataD "()" [] [Con "()" []],
    tuple 2, tuple 3, tuple 4,
    DataD "[]" [NormalTV "a"] [Con "[]" [], Con ":" [VarT "a", listT (VarT "a")]]
    ]

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

integerE :: Integer -> Exp
integerE i = LitE (IntegerL i)

charE :: Char -> Exp
charE c = LitE (CharL c)

stringE :: [Char] -> Exp
stringE [] = ConE (Sig "[]" (listT charT))
stringE s = listE (map charE s)

-- | (a b ... c)
appsE :: [Exp] -> Exp
appsE = foldl1 AppE

-- | Given (a b ... c), returns [a, b, ..., c]
unappsE :: Exp -> [Exp]
unappsE (AppE a b) = unappsE a ++ [b]
unappsE e = [e]

