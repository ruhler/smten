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
import Smten.HaskellF.Compile.Dec

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

