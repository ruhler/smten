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

module Seri.Target.Haskell.Builtins.Integer (
    integerH
  ) where

import qualified Language.Haskell.TH as H

import Seri.Failable
import Seri.Lambda
import Seri.Target.Haskell.Compiler

integerH :: HCompiler
integerH =
  let me _ e = fail $ "integerH does not apply to exp: " ++ pretty e

      mt _ (ConT "Integer") = return $ H.ConT (H.mkName "Prelude.Integer")
      mt _ t = fail $ "integerH does not apply to type: " ++ pretty t

      prim :: HCompiler -> TopSig -> H.Exp -> Failable [H.Dec]
      prim c (TopSig nm _ t) b = do
        t' <- compile_type c c t
        let hsn = hsName nm
        let sig = H.SigD hsn t'
        let val = H.FunD hsn [H.Clause [] (H.NormalB b) []]
        return [sig, val]

      vare :: String -> H.Exp
      vare n = H.VarE (H.mkName n)

      bprim :: HCompiler -> TopSig -> String -> Failable [H.Dec]
      bprim c (TopSig nm _ t) b = do    
        t' <- compile_type c c t
        let hsn = hsName nm
        let sig = H.SigD hsn t'
        let val = H.FunD hsn [H.Clause
                [H.VarP (H.mkName "a"), H.VarP (H.mkName "b")]
                    (H.NormalB (
                        H.CondE (H.AppE (H.AppE (vare b) (vare "a")) (vare "b"))
                                (H.ConE (H.mkName "True"))
                                (H.ConE (H.mkName "False"))
                    )) []]
        return [sig, val]

      md c (PrimD s@(TopSig "Seri.Lib.Prelude.__prim_add_Integer" _ _)) = prim c s (vare "Prelude.+")
      md c (PrimD s@(TopSig "Seri.Lib.Prelude.__prim_sub_Integer" _ _)) = prim c s (vare "Prelude.-")
      md c (PrimD s@(TopSig "Seri.Lib.Prelude.__prim_mul_Integer" _ _)) = prim c s (vare "Prelude.*")
      md c (PrimD s@(TopSig "Seri.Lib.Prelude.<" _ _)) = bprim c s "Prelude.<"
      md c (PrimD s@(TopSig "Seri.Lib.Prelude.>" _ _)) = bprim c s "Prelude.>"
      md c (PrimD s@(TopSig "Seri.Lib.Prelude.__prim_eq_Integer" _ _)) = bprim c s "Prelude.=="

      -- TODO: why is eq_Char here? Why not have it in Char.hs or just merge
      -- everything together into one?
      md c (PrimD s@(TopSig "Seri.Lib.Prelude.__prim_eq_Char" _ _)) = bprim c s "Prelude.=="
      md _ d = fail $ "integerH does not apply to dec: " ++ pretty d
  in Compiler me mt md

