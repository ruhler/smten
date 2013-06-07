
-- TODO: these things should be specified by the user as annotations of some
-- form in the smten code. Currently they aren't because I haven't figured out
-- what form of annotation to use.
module Smten.CodeGen.Annotates (primdatas) where

import Smten.Name

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


