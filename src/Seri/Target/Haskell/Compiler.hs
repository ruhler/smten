
module Seri.Target.Haskell.Compiler (
    HCompiler,
    module Seri.Target.Compiler) where

import qualified Language.Haskell.TH.PprLib as H
import qualified Language.Haskell.TH as H

import Seri.Target.Compiler

type HCompiler = Compiler H.Exp H.Type H.Dec

