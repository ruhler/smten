
{-# LANGUAGE PatternGuards #-}

-- | Abstract constructors and deconstructors dealing with ExpH
module Smten.ExpH.Sugar (
    litEH, de_litEH, varEH, de_varEH, conEH, de_conEH, de_kconEH,
    appEH, appsEH,
    lamEH, letEH, aconEH,
    errorEH, de_errorEH,
    caseEH,
    ifEH, impliesEH,

    unitEH,
    boolEH, trueEH, falseEH, de_boolEH,
    integerEH, de_integerEH, bitEH, de_bitEH,
    charEH, de_charEH, de_tupleEH,
    ioEH, de_ioEH,
    pushfun, smttype,
    transform,
    ) where

import System.IO.Unsafe

import Control.Monad

import Data.List(genericLength)
import Data.Maybe(isJust)
import Data.IORef
import qualified Data.Map as Map

import Smten.Bit
import Smten.Lit
import Smten.Name
import Smten.Sig
import Smten.Type
import Smten.ExpH.ExpH
import Smten.ExpH.Typeof

-- Fully applied constructor
aconEH :: Name -> Type -> [ExpH] -> ExpH
aconEH n t args = identify $ \id -> ConEH id n t args

conEH :: Sig -> ExpH
conEH (Sig n t) =
 let coneh :: Name -> Type -> [ExpH] -> ExpH
     coneh n t args
        | Just (it, ot) <- de_arrowT t =
            lamEH (Sig (name "c") it) ot $ \x -> coneh n ot (args ++ [x])
        | otherwise = aconEH n t args
 in coneh n t []

-- Check for a fully applied constructor.
de_conEH :: ExpH -> Maybe (Name, Type, [ExpH])
de_conEH e
 | (ConEH _ n t xs) <- e = Just (n, t, xs)
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
 | LitEH l <- e = Just l
 | otherwise = Nothing

varEH :: Sig -> ExpH
varEH = VarEH

de_varEH :: ExpH -> Maybe Sig
de_varEH (VarEH s) = Just s
de_varEH _ = Nothing

appEH :: ExpH -> ExpH -> ExpH
appEH f x
 | LamEH _ (Sig _ t) _ g <- f = g x
 | IfEH _ a y n <- f =
    -- Perform Case Argument Pushing:
    -- (if a then y else n) x
    -- ===> (if a then y x else n x)
    let Just (_, t) = de_arrowT (typeof f)
    in letEH (Sig (name "_z") (typeof x)) t x $ \av ->
         ifEH a (appEH y av) (appEH n av)
 | ErrorEH t s <- f = 
    let Just (_, t) = de_arrowT (typeof f)
    in errorEH t s
 | otherwise = error "appEH"

smttype :: Type -> Bool
--smttype t = or [ t == boolT, t == integerT, isJust (de_bitT t) ]
smttype t = or [ t == boolT, isJust (de_bitT t) ]

appsEH :: ExpH -> [ExpH] -> ExpH
appsEH f xs = foldl appEH f xs

-- lamEH s t f
--  s - name and type of argument to function
--  t - output type of the function
lamEH :: Sig -> Type -> (ExpH -> ExpH) -> ExpH
lamEH s t f = identify $ \id -> LamEH id s t f

-- letEH s t v f
--  s - name and type of let variable
--  t - type of the let expression
--  v - value of the let variable
letEH :: Sig -> Type -> ExpH -> (ExpH -> ExpH) -> ExpH
letEH s t v b = appEH (lamEH s t b) v

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
 | ErrorEH t s <- e = Just (t, s)
 | otherwise = Nothing

ioEH :: IO ExpH -> ExpH
ioEH x = litEH (dynamicL x)

de_ioEH :: ExpH -> Maybe (IO ExpH)
de_ioEH x = do
    l <- de_litEH x
    de_dynamicL l

caseEH :: ExpH -> Sig -> ExpH -> ExpH -> ExpH
caseEH x k y n = {-# SCC "CASE_EH" #-} caseEH' x k y n

caseEH' :: ExpH -> Sig -> ExpH -> ExpH -> ExpH
caseEH' x k@(Sig nk _) y n
 | Just (s, _, vs) <- de_conEH x
    = if s == nk then appsEH y vs else n
 | Just (_, msg) <- de_errorEH x = errorEH (typeof n) msg
 | nk == name "True" = identify $ \id -> IfEH id x y n
 | nk == name "False" = identify $ \id -> IfEH id x n y
 | IfEH {} <- x =
    let f = lamEH (Sig (name "_x") (typeof x)) (typeof n) $ \x' ->
               caseEH x' k y n
    in pushfun f x
 | otherwise = error "caseEH"

-- Function pushing:
--    f (if x then y else n)
-- Turns into:
--    let _f = f in if x then _f y else _f n
pushfun :: ExpH -> ExpH -> ExpH
pushfun f e@(IfEH _ x y n) =
 let Just (_, ot) = de_arrowT $ typeof f
 in letEH (Sig (name "_f") (typeof f)) ot f $ \fv ->
      ifEH x (appEH fv y) (appEH fv n)

ifEH :: ExpH -> ExpH -> ExpH -> ExpH
ifEH p a b = caseEH p (Sig (name "True") boolT) a b

impliesEH :: ExpH -> ExpH -> ExpH
impliesEH p q = ifEH p q trueEH

-- Perform a generic transformation on an expression.
-- Applies the given function to each subexpression. Any matching
-- subexpression is replaced with the returned value, otherwise it continues
-- to recurse.
transform :: (ExpH -> Maybe ExpH) -> ExpH -> ExpH
transform g e =
  let {-# NOINLINE cache #-}
      cache :: IORef (Map.Map EID ExpH, ExpH -> Maybe ExpH)
      cache = unsafePerformIO (newIORef (Map.empty, g))

      lookupIO :: EID -> ExpH -> IO ExpH
      lookupIO x e = do
        m <- readIORef cache
        case Map.lookup x (fst m) of
          Just v -> return v    
          Nothing -> do
            let v = def e
            modifyIORef cache $ \(m, g) -> (Map.insert x v m, g)
            return v

      lookupPure :: EID -> ExpH -> ExpH
      lookupPure x e = unsafePerformIO (lookupIO x e)

      def :: ExpH -> ExpH
      def e
        | Just v <- g e = v
        | LitEH {} <- e = e
        | ConEH _ n s xs <- e = identify $ \id -> ConEH id n s (map use xs)
        | VarEH {} <- e = e
        | PrimEH _ _ _ f xs <- e = f (map use xs)
        | LamEH _ s t f <- e = lamEH s t $ \x -> use (f x)
        | IfEH _ x y d <- e = ifEH (use x) (use y) (use d)
        | ErrorEH {} <- e = e

      use :: ExpH -> ExpH
      use e
        | Just x <- getid e = lookupPure x e
        | otherwise = def e
  in def e

de_tupleEH :: ExpH -> Maybe [ExpH]
de_tupleEH x = 
    case de_conEH x of
       Just (nm, _, xs) -> do
          n <- de_tupleN nm
          guard $ genericLength xs == n
          return xs
       _ -> Nothing

