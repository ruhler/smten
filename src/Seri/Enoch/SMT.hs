
module Seri.Enoch.SMT (
 ) where

import Control.Monad.State.Strict

data Answer a =
    Satisfiable a
  | Unsatisfiable
  | Unknown

instance (Seriable a) => Seriable (Answer a) where
    pack (Satisfiable x) =
      let ct = arrowsT [serit x, ConT "Answer"]
          con = ConE (Sig "Seri.SMT.SMT.Satisfiable" ct)
      in apply con (pack x)

    unpack (AppE (ConE (Sig "Seri.SMT.SMT.Satisfiable" _)) x) = do
        a <- unpack x
        return $ Satisfiable a
    unpack (ConE (Sig "Seri.SMT.SMT.Unsatisfiable" _)) = return Unsatisfiable
    unpack (ConE (Sig "Seri.SMT.SMT.Unknown" _)) = return Unknown

    serit = ConT "Answer"
                

type Query = State QS

free :: Query (TExp a)
free = run (VarE (Sig "Seri.SMT.SMT.free" ()))

assert :: TExp Bool -> Query ()
assert p = run' (apply (VarE (Sig "Seri.SMT.SMT.assert" ())) p)

query :: (Seriable a) => TExp a -> Query (Answer a)
query x = run' (apply (VarE (Sig "Seri.SMT.SMT.query" ())) x)

run :: TExp (Query a) -> Query (TExp a)
run (TExp x) = runQueryM x

run' :: TExp (Query a) -> Query a
run' x = fmap unpack' $ run x
    
    

