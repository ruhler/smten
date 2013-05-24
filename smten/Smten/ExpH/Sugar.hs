
{-# LANGUAGE PatternGuards #-}

-- Something about strictness analysis in this file leads to space leaks.
-- So don't allow the strictness analysis here.
{-# OPTIONS_GHC -fno-strictness #-}

-- | Abstract constructors and deconstructors dealing with ExpH
module Smten.ExpH.Sugar (
    litEH, de_litEH, varEH, de_varEH, conEH, de_conEH, de_kconEH,
    appEH, appsEH, strict_appEH,
    lamEH, letEH, aconEH,
    caseEH,
    ifEH, impliesEH, notEH, andEH, errorEH,

    unitEH,
    boolEH, trueEH, falseEH, de_boolEH,
    integerEH, de_integerEH, bitEH, de_bitEH,
    charEH, de_charEH, de_tupleEH,
    ioEH, de_ioEH,
    smttype,
    transform, shared,
    ) where

import System.IO.Unsafe

import Control.Monad
import Control.Monad.IO.Class

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
aconEH :: Name -> Type -> [ExpH] -> ExpH
aconEH n t args = exph $ ConEH n t args

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
 | (ConEH n t xs) <- force e = Just (n, t, xs)
 | otherwise = Nothing

-- Check for the given fully applied constructor.
de_kconEH :: Name -> ExpH -> Maybe [ExpH]
de_kconEH n x = do
    (nm, _, vs) <- de_conEH x
    guard $ nm == n
    return vs

litEH :: Lit -> ExpH
litEH = exph . LitEH

de_litEH :: ExpH -> Maybe Lit
de_litEH e
 | LitEH l <- force e = Just l
 | otherwise = Nothing

varEH :: Sig -> ExpH
varEH = exph . VarEH

de_varEH :: ExpH -> Maybe Sig
de_varEH t
 | VarEH s <- force t = Just s
 | otherwise = Nothing

appEH :: ExpH -> ExpH -> ExpH
appEH f x
 | LamEH _ _ g <- force f = g x
 | IfEH ft _ _ _ <- force f =
     let Just (_, t) = de_arrowT ft
     in strict_appEH t (\g -> appEH g x) f
 | ErrorEH ft s <- force f =
     let Just (_, t) = de_arrowT ft
     in errorEH t s
 | otherwise = error "SMTEN INTERNAL ERROR: unexpected arg to appEH"

smttype :: Type -> Bool
--smttype t = or [ t == boolT, t == integerT, isJust (de_bitT t) ]
smttype t = or [ t == boolT, isJust (de_bitT t) ]

appsEH :: ExpH -> [ExpH] -> ExpH
appsEH f xs = foldl appEH f xs

-- lamEH s t f
--  s - name and type of argument to function
--  t - output type of the function
lamEH :: Sig -> Type -> (ExpH -> ExpH) -> ExpH
lamEH s t f = exph $ LamEH s t f

-- letEH s t v f
--  s - name and type of let variable
--  t - type of the let expression
--  v - value of the let variable
letEH :: Sig -> Type -> ExpH -> (ExpH -> ExpH) -> ExpH
letEH s t v b = appEH (lamEH s t b) v

unitEH :: ExpH
unitEH = conEH (Sig unitN unitT)

trueEH :: ExpH
trueEH = conEH (Sig trueN boolT)

falseEH :: ExpH
falseEH = conEH (Sig falseN boolT)

-- | Boolean expression
boolEH :: Bool -> ExpH
boolEH True = trueEH
boolEH False = falseEH

de_boolEH :: ExpH -> Maybe Bool
de_boolEH x =
 let detrue = de_kconEH trueN x >> return True
     defalse = de_kconEH falseN x >> return False
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

ioEH :: IO ExpH -> ExpH
ioEH x = litEH (dynamicL x)

de_ioEH :: ExpH -> Maybe (IO ExpH)
de_ioEH x = do
    l <- de_litEH x
    de_dynamicL l

caseEH :: Type -> ExpH -> Sig -> ExpH -> ExpH -> ExpH
caseEH t x k@(Sig nk _) y n
 | nk == trueN = ifEH t x y n
 | nk == falseN = ifEH t x n y
 | otherwise =
  case force x of
    ConEH k _ vs -> if k == nk then appsEH y vs else n
    ErrorEH _ s -> errorEH t s
    IfEH {} -> strict_appEH t (\x' -> caseEH t x' k y n) x
    _ -> error $ "SMTEN INTERNAL ERROR: unexpected arg to caseEH"

-- Strict application.
-- It traverses inside of if expressions. Sharing is preserved.
strict_appEH :: Type -> (ExpH -> ExpH) -> ExpH -> ExpH
strict_appEH t f =
  let g :: (ExpH -> ExpH) -> ExpH -> ExpH
      g use e
        | IfEH _ x y d <- force e = ifEH t x (use y) (use d)
        | otherwise = f e
  in shared g

ifEH :: Type -> ExpH -> ExpH -> ExpH -> ExpH
ifEH t p a b =
  case force p of
    ConEH k _ []
      | k == trueN -> a
      | k == falseN -> b
    ErrorEH _ s -> errorEH t s
    _ -> exph $ IfEH t p a b

impliesEH :: ExpH -> ExpH -> ExpH
impliesEH p q = ifEH boolT p q trueEH

notEH :: ExpH -> ExpH
notEH p = ifEH boolT p falseEH trueEH

andEH :: ExpH -> ExpH -> ExpH
andEH p q = ifEH boolT p q falseEH

-- Perform a generic transformation on an expression.
-- Applies the given function to each subexpression. Any matching
-- subexpression is replaced with the returned value, otherwise it continues
-- to recurse.
--
-- Note: The transformation should NOT change the type of the expression.
transform :: (ExpH -> Maybe ExpH) -> ExpH -> ExpH
transform f =
  let g :: (ExpH -> ExpH) -> ExpH -> ExpH
      g use e
        | Just v <- f e = v
        | LitEH {} <- force e = e
        | ConEH n s xs <- force e = aconEH n s (map use xs)
        | VarEH {} <- force e = e
        | PrimEH _ _ f xs <- force e = f (map use xs)
        | LamEH s t f <- force e = lamEH s t $ \x -> use (f x)
        | IfEH t x y d <- force e = ifEH t (use x) (use y) (use d)
        | ErrorEH {} <- force e = e
  in shared g

de_tupleEH :: ExpH -> Maybe [ExpH]
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
shared :: ((ExpH -> a) -> ExpH -> a) -> ExpH -> a
shared f = 
  let {-# NOINLINE cache #-}
      -- Note: the IORef is a pair of map instead of just the map to ensure we
      -- get a new IORef every time the 'shared' function is called.
      --cache :: IORef (Map.Map EID a, ((ExpH -> a) -> ExpH -> a))
      cache = unsafeDupablePerformIO (newIORef (Map.empty, f))

      --lookupIO :: EID -> ExpH -> IO a
      lookupIO x e = do
        m <- readIORef cache
        case Map.lookup x (fst m) of
          Just v -> return v    
          Nothing -> do
            let v = def e
            -- TODO: Do we need to make the insert strict to avoid space leaks?
            modifyIORef cache $ \(m, g) -> (Map.insert x v m, g)
            return v

      --lookupPure :: EID -> ExpH -> a
      lookupPure x e = unsafeDupablePerformIO (lookupIO x e)

      --def :: ExpH -> a
      def = f use

      --use :: ExpH -> a
      use e = lookupPure (eid e) e
  in def

errorEH :: Type -> String -> ExpH
errorEH t s = exph $ ErrorEH t s

