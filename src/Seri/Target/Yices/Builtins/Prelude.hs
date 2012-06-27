
module Seri.Target.Yices.Builtins.Prelude (preludeY) where

import Seri.Target.Yices.Compiler

import Seri.Target.Yices.Builtins.Integer

preludeY :: YCompiler
preludeY = compilers [integerY]

