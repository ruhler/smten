
module Seri.Target.Yices.Builtins.Prelude (preludeY) where

import Seri.Target.Yices.Compiler

import Seri.Target.Yices.Builtins.Bool
import Seri.Target.Yices.Builtins.Integer

preludeY :: Compiler
preludeY = compilers [boolY, integerY]

