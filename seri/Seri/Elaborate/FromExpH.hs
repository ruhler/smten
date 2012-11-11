
module Seri.Elaborate.FromExpH (
    fromExpH
  ) where

import Seri.Lambda
import Seri.Elaborate.ExpH
import Seri.Elaborate.FreshPretty

fromExpH :: ExpH -> Exp
fromExpH e = runFresh (fromExpHM e)

-- Translate back to the normal Exp representation
fromExpHM :: ExpH -> Fresh Exp
fromExpHM (LitEH l) = return (LitE l)
fromExpHM (ConEH s) = return (ConE s)
fromExpHM (VarEH s) = return (VarE s)
fromExpHM e@(AppEH {}) = do
  (f:args) <- mapM fromExpHM (unappsEH e)
  return (AppE f args)
fromExpHM (LamEH _ s f) = do
  s' <- fresh s
  b <- fromExpHM (f (VarEH s'))
  return (lamE $ Match [VarP s'] b)
fromExpHM (CaseEH _ arg (Sig n t) yes no) = do
  arg' <- fromExpHM arg
  let tys = unarrowsT t
  vars <- mapM (fresh . Sig (name "_x")) (init tys)
  yes' <- fromExpHM (appEH yes (map VarEH vars))
  no' <- fromExpHM no
  let pt = typeof arg'
  return $ caseE arg' [Match [ConP pt n (map VarP vars)] yes',
                       Match [WildP pt] no']

