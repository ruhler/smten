
{-# LANGUAGE PatternGuards #-}

-- | Abstract constructors and deconstructors dealing with ExpH
module Seri.ExpH.Sugar (
    litEH, de_litEH, varEH, de_varEH, conEH, de_conEH, de_kconEH,
    appEH, de_appEH, appsEH, de_appsEH,
    lamEH, letEH, de_letEH, un_letEH,
    errorEH, de_errorEH,
    caseEH, ifEH,

    unitEH,
    boolEH, trueEH, falseEH, de_boolEH,
    integerEH, de_integerEH, bitEH, de_bitEH,
    charEH, de_charEH,
    ioEH, de_ioEH,
    pushfun, smttype,
    transform,
    ) where

import Control.Monad

import Data.Maybe(isJust)

import Seri.Bit
import Seri.Lit
import Seri.Name
import Seri.Sig
import Seri.Type
import Seri.ExpH.ExpH
import Seri.ExpH.Typeof

conEH :: Sig -> ExpH
conEH (Sig n t) =
 let coneh :: Name -> Type -> [ExpH] -> ExpH
     coneh n t args
        | Just (it, ot) <- de_arrowT t =
            LamEH (Sig (name "c") it) ot $ \x -> coneh n ot (args ++ [x])
        | otherwise = ConEH n t args
 in coneh n t []

-- Check for a fully applied constructor.
de_conEH :: ExpH -> Maybe (Name, Type, [ExpH])
de_conEH e
 | (ConEH n t xs) <- un_letEH e = Just (n, t, xs)
 | otherwise = Nothing

-- Check for the given fully applied constructor.
de_kconEH :: Name -> ExpH -> Maybe [ExpH]
de_kconEH n x = do
    (nm, _, vs) <- de_conEH x
    guard $ nm == n
    return vs

litEH :: Lit -> ExpH
litEH = LitEH

de_litEH :: ExpH -> Maybe Lit
de_litEH e
 | LitEH l <- un_letEH e = Just l
 | otherwise = Nothing

varEH :: Sig -> ExpH
varEH = VarEH

de_varEH :: ExpH -> Maybe Sig
de_varEH (VarEH s) = Just s
de_varEH _ = Nothing

-- We don't apply lambdas here if the arguments are supported by SMT. That's
-- done lazily in de_litEH, de_conEH, and de_errorEH. This is to preserve
-- sharing as much as possible.
appEH :: ExpH -> ExpH -> ExpH
appEH f x
 | LamEH (Sig _ t) _ g <- un_letEH f = g x
 | CaseEH a k y n <- un_letEH f =
    -- Perform Case Argument Pushing:
    -- (case a of { k -> y ; _ -> n}) x
    --  where y = \v1 -> \v2 -> ... -> y v
    -- ===> (case a of { k -> \v1 -> \v2 -> ... -> yv x; _ -> n x })
    let kargs = length (de_arrowsT (typeof k)) - 1
        Just (_, t) = de_arrowT (typeof f)
    in letEH (Sig (name "_z") (typeof x)) t x $ \av ->
         let g x = appEH x av
             y' = onyv kargs g y
             n' = g n
         in caseEH a k y' n'
 | otherwise = let e = AppEH f x e in e

smttype :: Type -> Bool
--smttype t = or [ t == boolT, t == integerT, isJust (de_bitT t) ]
smttype t = or [ t == boolT, isJust (de_bitT t) ]

de_appEH :: ExpH -> Maybe (ExpH, ExpH)
de_appEH (AppEH f x _) = Just (f, x)
de_appEH _ = Nothing

appsEH :: ExpH -> [ExpH] -> ExpH
appsEH f xs = foldl appEH f xs

de_appsEH :: ExpH -> (ExpH, [ExpH])
de_appsEH (AppEH a b _) =
    let (f, as) = de_appsEH a
    in (f, as ++ [b])
de_appsEH t = (t, [])

-- lamEH s t f
--  s - name and type of argument to function
--  t - output type of the function
lamEH :: Sig -> Type -> (ExpH -> ExpH) -> ExpH
lamEH = LamEH

-- letEH s t v f
--  s - name and type of let variable
--  t - type of the let expression
--  v - value of the let variable
letEH :: Sig -> Type -> ExpH -> (ExpH -> ExpH) -> ExpH
letEH s t v b = appEH (lamEH s t b) v

-- (s, t, v, f)
--  s - name and type of let variable
--  t - type of let body
--  v - value of let variable
de_letEH :: ExpH -> Maybe (Sig, Type, ExpH, ExpH -> ExpH)
de_letEH (AppEH f v _)
  | LamEH s t b <- un_letEH f = Just (s, t, v, b)
de_letEH _ = Nothing

-- Remove all lets from the given expression.
un_letEH :: ExpH -> ExpH
un_letEH (AppEH _ _ x) = x
un_letEH x = x

unitEH :: ExpH
unitEH = conEH (Sig (name "()") unitT)

trueEH :: ExpH
trueEH = conEH (Sig (name "True") boolT)

falseEH :: ExpH
falseEH = conEH (Sig (name "False") boolT)

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
de_errorEH e
 | ErrorEH t s <- un_letEH e = Just (t, s)
 | otherwise = Nothing

ioEH :: IO ExpH -> ExpH
ioEH x = litEH (dynamicL x)

de_ioEH :: ExpH -> Maybe (IO ExpH)
de_ioEH x = do
    l <- de_litEH x
    de_dynamicL l

caseEH :: ExpH -> Sig -> ExpH -> ExpH -> ExpH
caseEH x k@(Sig nk _) y n
 | Just (s, _, vs) <- de_conEH x
    = if s == nk then appsEH y vs else n
 | Just (_, msg) <- de_errorEH x = errorEH (typeof n) msg
 | CaseEH {} <- x =
    let f = lamEH (Sig (name "_x") (typeof x)) (typeof n) $ \x' ->
               caseEH x' k y n
    in pushfun f x
 | VarEH (Sig nm t) <- x
 , t == boolT
 , Just kv <- de_boolEH (conEH k) =
    let g :: Bool -> ExpH -> Maybe ExpH
        g b e = do
            Sig nm' _ <- de_varEH e
            guard $ nm' == nm
            return (boolEH b)
    in CaseEH x k (transform (g kv) y) (transform (g (not kv)) n)
 | otherwise = CaseEH x k y n

-- Function pushing:
--    f (case x of { k -> y; _ -> n})
-- Turns into:
--    let _f = f in case x of { k -> _f y; _ -> _f n}
pushfun :: ExpH -> ExpH -> ExpH
pushfun f (CaseEH x k y n) =
 let kargs = length (de_arrowsT (typeof k)) - 1
     Just (_, ot) = de_arrowT $ typeof f
     g yv = appEH 
 in letEH (Sig (name "_f") (typeof f)) ot f $ \fv ->
      let g x = appEH fv x
          y' = onyv kargs g y
          n' = g n
      in caseEH x k y' n'

ifEH :: ExpH -> ExpH -> ExpH -> ExpH
ifEH p a b = caseEH p (Sig (name "True") boolT) a b

-- For a case expresion, we have:
-- onyv n f y
--  where y = \v1 -> \v2 -> ... -> \vn -> yv
-- Returns: y' = \v1 -> \v2 -> ... -> \vn -> f yv
onyv :: Int -> (ExpH -> ExpH) -> ExpH -> ExpH
onyv 0 f yv = f yv
onyv n f (LamEH s t b) =
  let ts = de_arrowsT t
      (its, fot) = splitAt n ts
      fot' = arrowsT $ tail fot
      ot = arrowsT (its ++ [fot'])
  in lamEH s ot $ \x -> onyv (n-1) f (b x)

-- Perform a generic transformation on an expression.
-- Applies the given function to each subexpression. Any matching
-- subexpression is replaced with the returned value, otherwise it continues
-- to recurse.
transform :: (ExpH -> Maybe ExpH) -> ExpH -> ExpH
transform g e | Just v <- g e = v
transform g e =
  let me = transform g
  in case e of
       LitEH {} -> e
       ConEH n s xs -> ConEH n s (map me xs)
       VarEH {} -> e 
       PrimEH _ _ f xs -> f (map me xs)
       AppEH f x i -> AppEH (me f) (me x) (me i)
       LamEH s t f -> lamEH s t $ \x -> me (f x)
       CaseEH x k y d -> caseEH (me x) k (me y) (me d)
       ErrorEH {} -> e

