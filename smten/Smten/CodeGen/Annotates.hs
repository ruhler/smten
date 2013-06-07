
-- TODO: these things should be specified by the user as annotations of some
-- form in the smten code. Currently they aren't because I haven't figured out
-- what form of annotation to use.
module Smten.CodeGen.Annotates (haskellys, primdatas) where

import Smten.Name

-- For the given Data definition, generate an instance of Haskelly assuming
-- the Haskell version is defined in the given haskell module.
haskellys :: [(Name, String)]
haskellys = [
    (unitN, "Prelude"),
    (listN, "Prelude"),
    (maybeN, "Prelude"),
    (name "Smten.Symbolic.Solver", "Smten.SMT.Solvers")
    ]

-- For the given type constructor, import its definition from the given
-- Haskell module rather than defining it ourselves.
primdatas :: [(Name, String)]
primdatas = [
  (boolN, "Smten.Runtime.SmtenHS"),
  (ioN, "Smten.Runtime.IO"),
  (charN, "Smten.Runtime.Char"),
  (integerN, "Smten.Runtime.SmtenHS"),
  (name "Smten.Bit.Dynamic.Bit", "Smten.Runtime.SmtenHS"),
  (name "Data.Array.PrimArray", "Smten.Runtime.Array"),
  (name "Smten.Symbolic.Symbolic", "Smten.Runtime.Symbolic")
  ]


