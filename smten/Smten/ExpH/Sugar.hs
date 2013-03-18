
{-# LANGUAGE PatternGuards #-}

-- | Abstract constructors and deconstructors dealing with ExpH
module Smten.ExpH.Sugar (
    litEH, de_litEH, varEH, de_varEH, conEH, de_conEH, de_kconEH,
    appEH, appsEH, strict_appEH,
    lamEH, letEH, aconEH,
    caseEH,
    ifEH, impliesEH, notEH, andEH,

    unitEH,
    boolEH, trueEH, falseEH, de_boolEH,
    integerEH, de_integerEH, bitEH, de_bitEH,
    charEH, de_charEH, de_tupleEH,
    ioEH, de_ioEH,
    smttype,
    transform,
    ) where

import System.IO.Unsafe

import Control.Monad

import Data.Functor((<$>))
import Data.List(genericLength)
import Data.Maybe(isJust)
import Data.IORef
import qualified Data.HashMap as Map

import Smten.Bit
import Smten.Lit
import Smten.Name
import Smten.Sig
import Smten.Type
import Smten.ExpH.ExpH
import Smten.ExpH.Typeof

-- Fully applied constructor
aconEH :: Name -> Type -> [Thunk] -> Thunk
aconEH n t [] = thunkNS $ ConEH n t []
aconEH n t args = thunk $ ConEH n t args

conEH :: Sig -> Thunk
conEH (Sig n t) =
 let coneh :: Name -> Type -> [Thunk] -> Thunk
     coneh n t args
        | Just (it, ot) <- de_arrowT t =
            lamEH (Sig (name "c") it) ot $ \x -> coneh n ot (args ++ [x])
        | otherwise = aconEH n t args
 in coneh n t []

-- Check for a fully applied constructor.
de_conEH :: Thunk -> Maybe (Name, Type, [Thunk])
de_conEH e
 | (ConEH n t xs) <- force e = Just (n, t, xs)
 | otherwise = Nothing

-- Check for the given fully applied constructor.
de_kconEH :: Name -> Thunk -> Maybe [Thunk]
de_kconEH n x = do
    (nm, _, vs) <- de_conEH x
    guard $ nm == n
    return vs

litEH :: Lit -> Thunk
litEH = thunkNS . LitEH

de_litEH :: Thunk -> Maybe Lit
de_litEH e
 | LitEH l <- force e = Just l
 | otherwise = Nothing

varEH :: Sig -> Thunk
varEH = thunkNS . VarEH

de_varEH :: Thunk -> Maybe Sig
de_varEH t
 | VarEH s <- force t = Just s
 | otherwise = Nothing

appEH :: Thunk -> Thunk -> Thunk
appEH f x
 | LamEH (Sig _ t) _ g <- force f = g x
 | IfEH {} <- force f =
     let Just (_, t) = de_arrowT (typeof f)
     in strict_appEH t (\g -> appEH g x) f
 | otherwise = error "appEH"

smttype :: Type -> Bool
--smttype t = or [ t == boolT, t == integerT, isJust (de_bitT t) ]
smttype t = or [ t == boolT, isJust (de_bitT t) ]

appsEH :: Thunk -> [Thunk] -> Thunk
appsEH f xs = foldl appEH f xs

-- lamEH s t f
--  s - name and type of argument to function
--  t - output type of the function
lamEH :: Sig -> Type -> (Thunk -> Thunk) -> Thunk
lamEH s t f = thunk $ LamEH s t f

-- letEH s t v f
--  s - name and type of let variable
--  t - type of the let expression
--  v - value of the let variable
letEH :: Sig -> Type -> Thunk -> (Thunk -> Thunk) -> Thunk
letEH s t v b = appEH (lamEH s t b) v

unitEH :: Thunk
unitEH = conEH (Sig (name "()") unitT)

trueEH :: Thunk
trueEH = conEH (Sig (name "True") boolT)

falseEH :: Thunk
falseEH = conEH (Sig (name "False") boolT)

-- | Boolean expression
boolEH :: Bool -> Thunk
boolEH True = trueEH
boolEH False = falseEH

de_boolEH :: Thunk -> Maybe Bool
de_boolEH x =
 let detrue = de_kconEH (name "True") x >> return True
     defalse = de_kconEH (name "False") x >> return False
 in mplus detrue defalse

integerEH :: Integer -> Thunk
integerEH = litEH . integerL 


de_integerEH :: Thunk -> Maybe Integer
de_integerEH e = do
    l <- de_litEH e
    de_integerL l

bitEH :: Bit -> Thunk
bitEH = litEH . bitL

de_bitEH :: Thunk -> Maybe Bit
de_bitEH e = do
    l <- de_litEH e
    de_bitL l

charEH :: Char -> Thunk
charEH = litEH . charL 

de_charEH :: Thunk -> Maybe Char
de_charEH e = do
    l <- de_litEH e
    de_charL l

ioEH :: IO Thunk -> Thunk
ioEH x = litEH (dynamicL x)

de_ioEH :: Thunk -> Maybe (IO Thunk)
de_ioEH x = do
    l <- de_litEH x
    de_dynamicL l

caseEH :: Type -> Thunk -> Sig -> Thunk -> Thunk -> Thunk
caseEH t x k y n = {-# SCC "CASE_EH" #-} caseEH' t x k y n

caseEH' :: Type -> Thunk -> Sig -> Thunk -> Thunk -> Thunk
caseEH' t x k@(Sig nk _) y n
 | Just (s, _, vs) <- de_conEH x
    = if s == nk then appsEH y vs else n
 | nk == name "True" = thunk $ IfEH t x y n
 | nk == name "False" = thunk $ IfEH t x n y
 | IfEH {} <- force x = strict_appEH t (\x' -> caseEH t x' k y n) x
 | otherwise = error "caseEH"

-- Strict application.
-- It traverses inside of if expressions. Sharing is preserved.
strict_appEH :: Type -> (Thunk -> Thunk) -> Thunk -> Thunk
strict_appEH t f =
  let g :: (Thunk -> Thunk) -> Thunk -> Thunk
      g use e
        | IfEH _ x y d <- force e = ifEH t x (use y) (use d)
        | otherwise = f e
  in shared g

ifEH :: Type -> Thunk -> Thunk -> Thunk -> Thunk
ifEH t p a b = caseEH t p (Sig (name "True") boolT) a b

impliesEH :: Thunk -> Thunk -> Thunk
impliesEH p q = ifEH boolT p q trueEH

notEH :: Thunk -> Thunk
notEH p = ifEH boolT p falseEH trueEH

andEH :: Thunk -> Thunk -> Thunk
andEH p q = ifEH boolT p q falseEH

-- Perform a generic transformation on an expression.
-- Applies the given function to each subexpression. Any matching
-- subexpression is replaced with the returned value, otherwise it continues
-- to recurse.
--
-- Note: The transformation should NOT change the type of the expression.
transform :: (Thunk -> Maybe Thunk) -> Thunk -> Thunk
transform f =
  let g :: (Thunk -> Thunk) -> Thunk -> Thunk
      g use e
        | Just v <- f e = v
        | LitEH {} <- force e = e
        | ConEH n s xs <- force e
            = (if null xs then thunkNS else thunk) $ ConEH n s (map use xs)
        | VarEH {} <- force e = e
        | PrimEH _ _ f xs <- force e = f (map use xs)
        | LamEH s t f <- force e = lamEH s t $ \x -> use (f x)
        | IfEH t x y d <- force e = ifEH t (use x) (use y) (use d)
  in shared g

de_tupleEH :: Thunk -> Maybe [Thunk]
de_tupleEH x = 
    case de_conEH x of
       Just (nm, _, xs) -> do
          n <- de_tupleN nm
          guard $ genericLength xs == n
          return xs
       _ -> Nothing

-- shared f
-- Apply a function to an ExpH which preserves sharing.
-- If the function is called multiple times on the same ExpH, it shares
-- the result.
--
-- f - The function to apply which takes:
--   f' - the shared version of 'f' to recurse with
--   x - the argument
shared :: ((Thunk -> a) -> Thunk -> a) -> Thunk -> a
shared f = 
  let {-# NOINLINE cache #-}
      -- Note: the IORef is a pair of map instead of just the map to ensure we
      -- get a new IORef every time the 'shared' function is called.
      --cache :: IORef (Map.Map EID a, ((Thunk -> a) -> Thunk -> a))
      cache = unsafePerformIO (newIORef (Map.empty, f))

      --lookupIO :: EID -> Thunk -> IO a
      lookupIO x e = do
        m <- readIORef cache
        case Map.lookup x (fst m) of
          Just v -> return v    
          Nothing -> do
            let v = def e
            -- TODO: Do we need to make the insert strict to avoid space leaks?
            modifyIORef cache $ \(m, g) -> (Map.insert x v m, g)
            return v

      --lookupPure :: EID -> Thunk -> a
      lookupPure x e = unsafePerformIO (lookupIO x e)

      --def :: Thunk -> a
      def = f use

      --use :: Thunk -> a
      use e
       | Just x <- eid e = lookupPure x e
       | otherwise = def e
  in def

