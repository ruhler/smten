
{-# LANGUAGE PatternGuards #-}

-- | Abstract constructors and deconstructors dealing with ExpH
module Seri.ExpH.Sugar (
    litEH, de_litEH, varEH, de_varEH, conEH, de_conEH,
    appEH, de_appEH, appsEH, de_appsEH,
    lamEH, caseEH,

    unitEH,
    boolEH, trueEH, falseEH, de_boolEH,
    integerEH, de_integerEH, bitEH, de_bitEH,
    charEH, de_charEH,

    de_notEH, de_andEH, de_orEH,
    integer_addEH, integer_subEH, integer_mulEH,
    integer_eqEH, integer_ltEH, integer_leqEH, integer_gtEH, 
    ) where

import Seri.Bit
import Seri.Lit
import Seri.Name
import Seri.Sig
import Seri.Type
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

lamEH :: Sig -> (ExpH -> ExpH) -> ExpH
lamEH = LamEH ES_None

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

-- Extract a Bit from an expression of the form: __prim_frominteger_Bit v
-- The expression should be elaborated already.
de_bitEH :: ExpH -> Maybe Bit
de_bitEH (AppEH _ (VarEH (Sig fib (AppT _ (AppT _ (NumT w))))) ve)
  | fib == name "Seri.Bit.__prim_fromInteger_Bit"
  , LitEH (IntegerL v) <- ve
  = Just (bv_make (nteval w) v)
de_bitEH _ = Nothing

charEH :: Char -> ExpH
charEH = litEH . CharL 

de_charEH :: ExpH -> Maybe Char
de_charEH e = do
    CharL c <- de_litEH e
    return c

caseEH :: ExpH -> Sig -> ExpH -> ExpH -> ExpH
caseEH x k@(Sig nk _) y n
 | (ConEH (Sig s _), vs) <- de_appsEH x
    = if s == nk then appsEH y vs else n
 | otherwise = CaseEH ES_None x k y n

integer_addEH :: ExpH -> ExpH -> ExpH
integer_addEH a b
  | Just av <- de_integerEH a
  , Just bv <- de_integerEH b = integerEH (av + bv)
  | otherwise = appsEH (varEH (Sig (name "Prelude.__prim_add_Integer") (arrowsT [integerT, integerT, integerT]))) [a, b]

integer_subEH :: ExpH -> ExpH -> ExpH
integer_subEH a b
  | Just av <- de_integerEH a
  , Just bv <- de_integerEH b = integerEH (av - bv)
  | otherwise = appsEH (varEH (Sig (name "Prelude.__prim_sub_Integer") (arrowsT [integerT, integerT, integerT]))) [a, b]

integer_mulEH :: ExpH -> ExpH -> ExpH
integer_mulEH a b
  | Just av <- de_integerEH a
  , Just bv <- de_integerEH b = integerEH (av * bv)
  | otherwise = appsEH (varEH (Sig (name "Prelude.__prim_mul_Integer") (arrowsT [integerT, integerT, integerT]))) [a, b]

integer_eqEH :: ExpH -> ExpH -> ExpH
integer_eqEH a b
  | Just av <- de_integerEH a
  , Just bv <- de_integerEH b = boolEH (av == bv)
  | otherwise = appsEH (varEH (Sig (name "Prelude.__prim_eq_Integer") (arrowsT [integerT, integerT, boolT]))) [a, b]

integer_ltEH :: ExpH -> ExpH -> ExpH
integer_ltEH a b
  | Just av <- de_integerEH a
  , Just bv <- de_integerEH b = boolEH (av < bv)
  | otherwise = appsEH (varEH (Sig (name "Prelude.<") (arrowsT [integerT, integerT, boolT]))) [a, b]

integer_leqEH :: ExpH -> ExpH -> ExpH
integer_leqEH a b
  | Just av <- de_integerEH a
  , Just bv <- de_integerEH b = boolEH (av <= bv)
  | otherwise = appsEH (varEH (Sig (name "Prelude.<=") (arrowsT [integerT, integerT, boolT]))) [a, b]

integer_gtEH :: ExpH -> ExpH -> ExpH
integer_gtEH a b
  | Just av <- de_integerEH a
  , Just bv <- de_integerEH b = boolEH (av > bv)
  | otherwise = appsEH (varEH (Sig (name "Prelude.>") (arrowsT [integerT, integerT, boolT]))) [a, b]

de_notEH :: ExpH -> Maybe ExpH
de_notEH e =
  case de_appsEH e of
    (VarEH (Sig n _), [x]) | n == name "Prelude.not" -> Just x
    _ -> Nothing

de_andEH :: ExpH -> Maybe (ExpH, ExpH)
de_andEH e =
  case de_appsEH e of
    (VarEH (Sig n _), [x, y]) | n == name "Prelude.and" -> Just (x, y)
    _ -> Nothing

de_orEH :: ExpH -> Maybe (ExpH, ExpH)
de_orEH e =
  case de_appsEH e of
    (VarEH (Sig n _), [x, y]) | n == name "Prelude.or" -> Just (x, y)
    _ -> Nothing

