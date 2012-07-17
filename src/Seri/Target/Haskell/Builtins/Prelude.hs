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

module Seri.Target.Haskell.Builtins.Prelude (
    preludeH
  ) where

import qualified Language.Haskell.TH as H

import Seri.Target.Haskell.Compiler
import Seri.Target.Haskell.Builtins.Integer

import Seri.Lambda

preludeH :: HCompiler
preludeH =
  let me _ e = fail $ "preludeH does not apply to exp: " ++ pretty e

      mt _ (ConT "Char") = return $ H.ConT (H.mkName "Prelude.Char")
      mt _ t = fail $ "preludeH does not apply to type: " ++ pretty t

      md _ (PrimD (TopSig "error" _ t)) = do
        let e = H.VarE (H.mkName "Prelude.error")
        let val = H.FunD (H.mkName "error") [H.Clause [] (H.NormalB e) []]
        return [val]
      md _ (DataD "Char" _ _) = return []
      md _ (DataD "Integer" _ _) = return []
      md _ (DataD "()" _ _) = return []
      md _ (DataD "(,)" _ _) = return []
      md _ (DataD "(,,)" _ _) = return []
      md _ (DataD "(,,,)" _ _) = return []
      md _ (DataD "[]" _ _) = return []
      md _ d = fail $ "preludeH does not apply to dec: " ++ pretty d
  in compilers [Compiler me mt md, integerH]

