
module Seri.Elaborate.FromExpH (
    fromExpH
  ) where

import Seri.Exp.Exp
import Seri.ExpH
import Seri.Elaborate.FreshFast

fromExpH :: ExpH -> Exp
fromExpH e = runFresh (fromExpHM e)

-- Translate back to the normal Exp representation
fromExpHM :: ExpH -> Fresh Exp
fromExpHM (LitEH l) = return (LitE l)
fromExpHM (ConEH s) = return (ConE s)
fromExpHM (VarEH s) = return (VarE s)
fromExpHM (AppEH _ f x) = do
    f' <- fromExpHM f
    x' <- fromExpHM x   
    return (AppE f' x')
fromExpHM (LamEH _ s f) = do
  s' <- fresh s
  b <- fromExpHM (f (VarEH s'))
  return (LamE s' b)
fromExpHM (CaseEH _ arg s yes no) = do
  arg' <- fromExpHM arg
  yes' <- fromExpHM yes
  no' <- fromExpHM no
  return $ CaseE arg' s yes' no'
