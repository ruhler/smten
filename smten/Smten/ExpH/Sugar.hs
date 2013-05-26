
{-# LANGUAGE PatternGuards #-}

-- Something about strictness analysis in this file leads to space leaks.
-- So don't allow the strictness analysis here.
{-# OPTIONS_GHC -fno-strictness #-}

-- | Abstract constructors and deconstructors dealing with ExpH
module Smten.ExpH.Sugar (
    litEH, de_litEH, varEH, de_varEH, de_conEH, de_kconEH,
    appEH, appsEH, strict_appEH,
    lamEH, conEH,
    caseEH,
    ifEH, impliesEH, notEH, andEH, errorEH,

    unitEH,
    boolEH, trueEH, falseEH, de_boolEH,
    integerEH, de_integerEH, bitEH, de_bitEH,
    charEH, de_charEH,
    ioEH, de_ioEH,
    smttype,
    transform, shared,
    ) where

import System.IO.Unsafe

import Control.Monad

import Data.Maybe(isJust)
import Data.IORef
import qualified Data.HashMap as Map

import Smten.Bit
import Smten.Lit
import Smten.Name
import Smten.Type
import Smten.ExpH.ExpH

-- Fully applied constructor
conEH :: Type -> Name -> [ExpH] -> ExpH
conEH t n args = exph t $ ConEH n args

-- Check for a fully applied constructor.
de_conEH :: ExpH -> Maybe (Name, [ExpH])
de_conEH e
 | (ConEH n xs) <- force e = Just (n, xs)
 | otherwise = Nothing

-- Check for the given fully applied constructor.
de_kconEH :: Name -> ExpH -> Maybe [ExpH]
de_kconEH n x = do
    (nm, vs) <- de_conEH x
    guard $ nm == n
    return vs

litEH :: Type -> Lit -> ExpH
litEH t = exph t . LitEH

de_litEH :: ExpH -> Maybe Lit
de_litEH e
 | LitEH l <- force e = Just l
 | otherwise = Nothing

varEH :: Type -> Name -> ExpH
varEH t = exph t . VarEH

de_varEH :: ExpH -> Maybe Name
de_varEH t
 | VarEH n <- force t = Just n
 | otherwise = Nothing

appEH :: Type -> ExpH -> ExpH -> ExpH
appEH t f x
 | LamEH _ g <- force f = g x
 | IfEH _ _ _ <- force f = strict_appEH t (\g -> appEH t g x) f
 | ErrorEH s <- force f = errorEH t s
 | otherwise = error "SMTEN INTERNAL ERROR: unexpected arg to appEH"

smttype :: Type -> Bool
--smttype t = or [ t == boolT, t == integerT, isJust (de_bitT t) ]
smttype t = or [ t == boolT, isJust (de_bitT t) ]

appsEH :: Type -> ExpH -> [ExpH] -> ExpH
appsEH _ x [] = x
appsEH t f (x:xs) =
 let ts = arrowsT (map typeof xs ++ [t])
 in appsEH t (appEH ts f x) xs

lamEH :: Type -> Name -> (ExpH -> ExpH) -> ExpH
lamEH t n f = exph t $ LamEH n f

unitEH :: ExpH
unitEH = conEH unitT unitN []

trueEH :: ExpH
trueEH = conEH boolT trueN []

falseEH :: ExpH
falseEH = conEH boolT falseN []

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
integerEH = litEH integerT . integerL 


de_integerEH :: ExpH -> Maybe Integer
de_integerEH e = do
    l <- de_litEH e
    de_integerL l

bitEH :: Type -> Bit -> ExpH
bitEH t = litEH t . bitL

de_bitEH :: ExpH -> Maybe Bit
de_bitEH e = do
    l <- de_litEH e
    de_bitL l

charEH :: Char -> ExpH
charEH = litEH charT . charL 

de_charEH :: ExpH -> Maybe Char
de_charEH e = do
    l <- de_litEH e
    de_charL l

ioEH :: Type -> IO ExpH -> ExpH
ioEH t x = litEH t (dynamicL x)

de_ioEH :: ExpH -> Maybe (IO ExpH)
de_ioEH x = do
    l <- de_litEH x
    de_dynamicL l

caseEH :: Type -> ExpH -> Name -> ExpH -> ExpH -> ExpH
caseEH t x k y n
 | k == trueN = ifEH t x y n
 | k == falseN = ifEH t x n y
 | otherwise =
  case force x of
    ConEH k2 vs -> if k == k2 then appsEH t y vs else n
    ErrorEH s -> errorEH t s
    IfEH {} -> strict_appEH t (\x' -> caseEH t x' k y n) x
    _ -> error $ "SMTEN INTERNAL ERROR: unexpected arg to caseEH"

-- Strict application.
-- It traverses inside of if expressions. Sharing is preserved.
strict_appEH :: Type -> (ExpH -> ExpH) -> ExpH -> ExpH
strict_appEH t f =
  let g :: (ExpH -> ExpH) -> ExpH -> ExpH
      g use e
        | IfEH x y d <- force e = ifEH t x (use y) (use d)
        | otherwise = f e
  in shared g

ifEH :: Type -> ExpH -> ExpH -> ExpH -> ExpH
ifEH t p a b =
  case force p of
    ConEH k []
      | k == trueN -> a
      | k == falseN -> b
    ErrorEH s -> errorEH t s
    _ -> exph t $ IfEH p a b

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
        | ConEH n xs <- force e = conEH (typeof e) n (map use xs)
        | VarEH {} <- force e = e
        | PrimEH _ f xs <- force e = f (map use xs)
        | LamEH n f <- force e = lamEH (typeof e) n $ \x -> use (f x)
        | IfEH x y d <- force e = ifEH (typeof e) (use x) (use y) (use d)
        | ErrorEH {} <- force e = e
  in shared g

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
errorEH t s = exph t $ ErrorEH s

