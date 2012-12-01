
module Seri.ExpH.FromExpH (
    fromExpH
  ) where

import Seri.Sig
import Seri.Name
import Seri.Type
import Seri.Exp.Exp
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar
import Seri.ExpH.SeriEHs
import Seri.Fresh

fromExpH :: ExpH -> Exp
fromExpH e = runFreshFast (fromExpHM e)

-- Translate back to the normal Exp representation
fromExpHM :: (Fresh f) => ExpH -> f Exp
fromExpHM (LitEH l) = return (LitE l)
fromExpHM (ConEH s) = return (ConE s)
fromExpHM (VarEH s) = return (VarE s)
fromExpHM (PrimEH s _ xs) = fromExpHM (appsEH (varEH s) xs)
fromExpHM (AppEH f x) = do
    f' <- fromExpHM f
    x' <- fromExpHM x   
    return (AppE f' x')
fromExpHM (LamEH s f) = do
  s' <- fresh s
  b <- fromExpHM (f (VarEH s'))
  return (LamE s' b)
fromExpHM (CaseEH arg s yes no) = do
  arg' <- fromExpHM arg
  yes' <- fromExpHM yes
  no' <- fromExpHM no
  return $ CaseE arg' s yes' no'
fromExpHM (ErrorEH t s)
  = fromExpHM $ appEH (varEH (Sig (name "Prelude.error") (arrowT stringT t))) (stringEH s) 

