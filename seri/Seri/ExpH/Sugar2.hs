
-- | More syntactic sugar for ExpH.
-- These make use of SeriEHs.
module Seri.ExpH.Sugar2 (
    ifEH, errorEH,
    ) where

import Seri.Sig
import Seri.Name
import Seri.Type.Type
import Seri.Type.Sugar
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar
import Seri.ExpH.SeriEH
import Seri.ExpH.SeriEHs
import Seri.ExpH.Typeof

errorEH :: Type -> String -> ExpH
errorEH t msg = appEH (varEH (Sig (name "Prelude.error") (arrowsT [stringT, t]))) (seriEH msg)

ifEH :: ExpH -> ExpH -> ExpH -> ExpH
ifEH p a b = 
  let false = CaseEH ES_None p (Sig (name "False") boolT) b (errorEH (typeof b) "if failed to match")
  in CaseEH ES_None p (Sig (name "True") boolT) a false

