
{-# LANGUAGE PatternGuards #-}

-- | Abstract constructors and deconstructors dealing with ExpH
module Smten.ExpH.Sugar (
    litEH, de_litEH, varEH, de_varEH, conEH, de_conEH, de_kconEH,
    appEH, appsEH, strict_appEH,
    lamEH, letEH, aconEH,
    caseEH,
    ifEH, impliesEH, notEH, andEH, errorEH, thunkEH,

    unitEH,
    boolEH, trueEH, falseEH, de_boolEH,
    integerEH, de_integerEH, bitEH, de_bitEH,
    charEH, de_charEH, de_tupleEH,
    ioEH, de_ioEH,
    smttype,
    transform, shared, sharedM,
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

appEH ::  ExpH -> ExpH -> ExpH
appEH f x = thunkEH $ appEH' f x

appEH' :: ExpH -> ExpH -> ExpH
appEH' f x
 | LamEH (Sig _ t) _ g <- force f = g x
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

ioEH :: IO ExpH -> ExpH
ioEH x = litEH (dynamicL x)

de_ioEH :: ExpH -> Maybe (IO ExpH)
de_ioEH x = do
    l <- de_litEH x
    de_dynamicL l

thunkEH :: ExpH -> ExpH
thunkEH = exph . ThunkEH

caseEH :: Type -> ExpH -> Sig -> ExpH -> ExpH -> ExpH
caseEH t x k y n = thunkEH $ caseEH' t x k y n

caseEH' :: Type -> ExpH -> Sig -> ExpH -> ExpH -> ExpH
caseEH' t x k@(Sig nk _) y n
 | Just (s, _, vs) <- de_conEH x
    = if s == nk then appsEH y vs else n
 | ErrorEH _ s <- force x = errorEH t s
 | nk == name "True" = exph $ IfEH t x y n
 | nk == name "False" = exph $ IfEH t x n y
 | IfEH {} <- force x = strict_appEH t (\x' -> caseEH t x' k y n) x
 | otherwise = error $ "SMTEN INTERNAL ERROR: unexpected arg to caseEH"

-- Strict application.
-- It traverses inside of if expressions. Sharing is preserved.
strict_appEH :: Type -> (ExpH -> ExpH) -> ExpH -> ExpH
strict_appEH t f =
  let g :: (ExpH -> ExpH) -> ExpH -> ExpH
      g use e = thunkEH (g' use e)

      g' :: (ExpH -> ExpH) -> ExpH -> ExpH
      g' use e
        | IfEH _ x y d <- force e = ifEH t x (use y) (use d)
        | otherwise = f e
  in shared g

ifEH :: Type -> ExpH -> ExpH -> ExpH -> ExpH
ifEH t p a b = caseEH t p (Sig (name "True") boolT) a b

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
      g use e = thunkEH (g' use e)

      g' :: (ExpH -> ExpH) -> ExpH -> ExpH
      g' use e
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
      cache = unsafePerformIO (newIORef (Map.empty, f))

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
      lookupPure x e = unsafePerformIO (lookupIO x e)

      --def :: ExpH -> a
      def = f use

      --use :: ExpH -> a
      use e = lookupPure (eid e) e
  in def

-- sharedM f
-- Apply a monadic function to an ExpH which preserves sharing.
-- If the function is called multiple times on the same ExpH, it shares
-- the result. The monadic actions are only performed the first time an
-- expression is seen.
--
-- TODO: This only shares results for non-simple expressions (for performance
-- reasons). Does that make sense to do in general?
--
-- f - The function to apply which takes:
--   f' - the shared version of 'f' to recurse with
--   x - the argument
sharedM :: (MonadIO m) => ((ExpH -> m a) -> ExpH -> m a) -> ExpH -> m a
sharedM f x = {-# SCC "SHARED_M" #-} do
  cache <- liftIO $ newIORef Map.empty
  let --use :: ExpH -> m a
      use e
       | issimple e = f use e
       | otherwise = do
        m <- liftIO $ readIORef cache
        case Map.lookup (eid e) m of
          Just v -> return v    
          Nothing -> do
            v <- f use e
            -- The presence of this SCC appears to fix a stack overflow.
            -- TODO: why? What's up with that?
            liftIO $ {-# SCC "SHARED_M_INSERT" #-}
                modifyIORef' cache (Map.insert (eid e) v)
            return v
  f use x

errorEH :: Type -> String -> ExpH
errorEH t s = exph $ ErrorEH t s

-- TODO: why is this function defined both here and in FromExpH?
issimple :: ExpH -> Bool
issimple e =
  case force e of
     LitEH {} -> True
     ConEH _ _ [] -> True
     ConEH {} -> False
     VarEH {} -> True
     PrimEH {} -> False
     LamEH {} -> False
     IfEH {} -> False
     ThunkEH {} -> error "issimple: unexpected ThunkEH"
     ErrorEH {} -> False
