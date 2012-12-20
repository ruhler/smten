
module Seri.ExpH.FromExpH (
    fromExpH
  ) where

import Seri.Sig
import Seri.Name
import Seri.Type
import Seri.Exp
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar
import Seri.ExpH.SeriEHs

-- Translate back to the normal Exp representation
fromExpH :: ExpH -> Exp
fromExpH (LitEH l) = LitE l
fromExpH (ConEH n t xs) = 
  let xs' = map fromExpH xs
      t' = arrowsT $ (map typeof xs') ++ [t]
  in appsE (ConE (Sig n t')) xs'
fromExpH (VarEH s) = VarE s
fromExpH (PrimEH n t _ xs) =
  let xs' = map fromExpH xs
      t' = arrowsT $ (map typeof xs') ++ [t]
  in appsE (VarE (Sig n t')) xs'
fromExpH (AppEH f x) =
  let f' = fromExpH f
      x' = fromExpH x   
  in AppE f' x'
fromExpH (LamEH (Sig nm t) _ f) =
  let s' = identify $ \x -> Sig (nm `nappend` (name (show x))) t
      b = fromExpH (f (VarEH s'))
  in LamE s' b
fromExpH (CaseEH arg s yes no) =
  let arg' = fromExpH arg
      yes' = fromExpH yes
      no' = fromExpH no
  in CaseE arg' s yes' no'
fromExpH (ErrorEH t s)
  = fromExpH $ appEH (varEH (Sig (name "Prelude.error") (arrowT stringT t))) (stringEH s) 

