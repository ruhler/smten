
-- | Abstract constructors and deconstructors dealing with ExpH
module Seri.ExpH.Sugar (
    varEH, de_varEH, conEH, appEH, de_appEH, appsEH, de_appsEH,
    ) where

import Seri.ExpH.ExpH
import Seri.Sig

conEH :: Sig -> ExpH
conEH = ConEH

varEH :: Sig -> ExpH
varEH = VarEH

de_varEH :: ExpH -> Maybe Sig
de_varEH (VarEH s) = Just s
de_varEH _ = Nothing

appEH :: ExpH -> ExpH -> ExpH
appEH f x = AppEH ES_None f x

de_appEH :: ExpH -> Maybe (ExpH, ExpH)
de_appEH (AppEH _ f x) = Just (f, x)
de_appEH _ = Nothing

appsEH :: ExpH -> [ExpH] -> ExpH
appsEH f xs = foldl appEH f xs

de_appsEH :: ExpH -> (ExpH, [ExpH])
de_appsEH (AppEH _ a b) =
    let (f, as) = de_appsEH a
    in (f, as ++ [b])
de_appsEH t = (t, [])

