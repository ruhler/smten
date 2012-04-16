
module Seri.Slice (sliceof) where

import Language.Haskell.TH

-- To slice haskell expressions into quoted seri, use the form
--   _s (...)
--
-- Where (...) is the expression to slice in.
--
sliceof :: Exp -> Maybe Exp
sliceof (AppE (VarE s) b) | s == mkName "_s" = Just b
sliceof _ = Nothing

