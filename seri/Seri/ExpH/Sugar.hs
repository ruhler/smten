
{-# LANGUAGE PatternGuards #-}

-- | Abstract constructors and deconstructors dealing with ExpH
module Seri.ExpH.Sugar (
    litEH, de_litEH, varEH, de_varEH, conEH, de_conEH, de_kconEH,
    appEH, de_appEH, appsEH, de_appsEH,
    lamEH,
    errorEH, de_errorEH,

    unitEH,
    boolEH, trueEH, falseEH, de_boolEH,
    integerEH, de_integerEH, bitEH, de_bitEH,
    charEH, de_charEH,
    ioEH, de_ioEH,
    ) where

import Control.Monad

import Seri.Bit
import Seri.Lit
import Seri.Name
import Seri.Sig
import Seri.Type
import Seri.ExpH.ExpH
import Seri.ExpH.Typeof

conEH :: Sig -> ExpH
conEH = ConEH

-- Check for a fully applied constructor.
de_conEH :: ExpH -> Maybe (Sig, [ExpH])
de_conEH e =
  case de_appsEH e of
     (ConEH s@(Sig _ t), vs) -> do
        guard $ length vs == (length (de_arrowsT t) - 1)
        return (s, vs)
     _ -> Nothing

-- Check for the given fully applied constructor.
de_kconEH :: Name -> ExpH -> Maybe [ExpH]
de_kconEH n x = do
    (Sig nm _, vs) <- de_conEH x
    guard $ nm == n
    return vs

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
appEH (LamEH _ f) x = f x
appEH f x = AppEH f x

de_appEH :: ExpH -> Maybe (ExpH, ExpH)
de_appEH (AppEH f x) = Just (f, x)
de_appEH _ = Nothing

appsEH :: ExpH -> [ExpH] -> ExpH
appsEH f xs = foldl appEH f xs

de_appsEH :: ExpH -> (ExpH, [ExpH])
de_appsEH (AppEH a b) =
    let (f, as) = de_appsEH a
    in (f, as ++ [b])
de_appsEH t = (t, [])

lamEH :: Sig -> (ExpH -> ExpH) -> ExpH
lamEH = LamEH

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
de_boolEH x =
 let detrue = de_kconEH (name "True") x >> return True
     defalse = de_kconEH (name "False") x >> return False
 in mplus detrue defalse

integerEH :: Integer -> ExpH
integerEH = litEH . integerL 


de_integerEH :: ExpH -> Maybe Integer
de_integerEH e = do
    l <- de_litEH e
    de_integerL l

bitEH :: Bit -> ExpH
bitEH = litEH . bitL

de_bitEH :: ExpH -> Maybe Bit
de_bitEH e = do
    l <- de_litEH e
    de_bitL l

charEH :: Char -> ExpH
charEH = litEH . charL 

de_charEH :: ExpH -> Maybe Char
de_charEH e = do
    l <- de_litEH e
    de_charL l

-- The type passed to errorEH should be the return type of error when applied
-- to a string.
errorEH :: Type -> String -> ExpH
errorEH = ErrorEH

de_errorEH :: ExpH -> Maybe (Type, String)
de_errorEH (ErrorEH t s) = Just (t, s)
de_errorEH _ = Nothing

ioEH :: IO ExpH -> ExpH
ioEH x = litEH (dynamicL x)

de_ioEH :: ExpH -> Maybe (IO ExpH)
de_ioEH x = do
    l <- de_litEH x
    de_dynamicL l

