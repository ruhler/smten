
-- | Abstract constructors and deconstructors dealing with ExpH
module Seri.ExpH.Sugar (
    litEH, de_litEH, varEH, de_varEH, conEH, de_conEH,
    appEH, de_appEH, appsEH, de_appsEH,

    unitEH,
    boolEH, trueEH, falseEH, de_boolEH,
    integerEH, de_integerEH, bitEH,
    charEH, de_charEH,

    ifEH, 
    ) where

import Seri.Bit
import Seri.Lit
import Seri.Name
import Seri.Sig
import Seri.Type.Sugar
import Seri.ExpH.ExpH

conEH :: Sig -> ExpH
conEH = ConEH

de_conEH :: ExpH -> Maybe Sig
de_conEH (ConEH s) = Just s
de_conEH _ = Nothing

litEH :: Lit -> ExpH
litEH = LitEH

de_litEH :: ExpH -> Maybe Lit
de_litEH (LitEH l) = Just l
de_litEH _ = Nothing

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

unitEH :: ExpH
unitEH = conEH (Sig (name "()") unitT)

trueEH :: ExpH
trueEH = ConEH (Sig (name "True") boolT)

falseEH :: ExpH
falseEH = ConEH (Sig (name "False") boolT)

-- | Boolean expression
boolEH :: Bool -> ExpH
boolEH True = trueEH
boolEH False = falseEH

de_boolEH :: ExpH -> Maybe Bool
de_boolEH x | x == trueEH = Just True
de_boolEH x | x == falseEH = Just False
de_boolEH _ = Nothing

integerEH :: Integer -> ExpH
integerEH = litEH . IntegerL 


de_integerEH :: ExpH -> Maybe Integer
de_integerEH (LitEH (IntegerL i)) = Just i
de_integerEH _ = Nothing

bitEH :: Bit -> ExpH
bitEH b = appEH (varEH (Sig (name "Seri.Bit.__prim_fromInteger_Bit") (arrowsT [integerT, bitT (bv_width b)]))) (integerEH $ bv_value b)

charEH :: Char -> ExpH
charEH = litEH . CharL 

de_charEH :: ExpH -> Maybe Char
de_charEH e = do
    CharL c <- de_litEH e
    return c

ifEH :: ExpH -> ExpH -> ExpH -> ExpH
ifEH p a b = 
  let false = CaseEH ES_None p (Sig (name "False") boolT) b (error "if failed to match")
  in CaseEH ES_None p (Sig (name "True") boolT) a false

