
{-# LANGUAGE PatternGuards #-}

-- | More syntactic sugar for ExpH.
-- These make use of SeriEHs.
module Seri.ExpH.Sugar2 (
    ifEH, errorEH, de_stringEH,
    ) where

import Seri.Sig
import Seri.Name
import Seri.Type
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar
import Seri.ExpH.SeriEH
import Seri.ExpH.SeriEHs
import Seri.ExpH.Typeof

errorEH :: Type -> String -> ExpH
errorEH t msg = appEH (varEH (Sig (name "Prelude.error") (arrowsT [stringT, t]))) (seriEH msg)

ifEH :: ExpH -> ExpH -> ExpH -> ExpH
ifEH p a b = caseEH p (Sig (name "True") boolT) a b

de_stringEH :: ExpH -> Maybe String
de_stringEH = de_seriEH

