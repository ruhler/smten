
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ExpH where

import System.IO.Unsafe
import Data.IORef

import Smten.Name
import Smten.Type
import Smten.Sig
import Smten.Lit
import Smten.Ppr

import qualified Data.HashMap as Map
import Data.Hashable

newtype EID = EID Integer
    deriving (Eq, Hashable, Ord)

-- ExpH_Value represents a symbolic expression evaluated to weak head normal
-- form, or a thunk.
data ExpH_Value =
            -- | Literal characters and integers
            LitEH Lit

            -- | Fully applied data constructors.
            -- The Type field is the type of the fully applied constructor.
          | ConEH Name Type [ExpH]

            -- | Primitive symbolic varibles.
            -- Current types supported are: Bool, Integer, Bit
          | VarEH Sig

            -- | Fully applied primitive functions
            -- | The Type field is the type of the fully applied primitive.
          | PrimEH Name Type ([ExpH] -> ExpH) [ExpH]
         
          -- | LamEH s t f:
          --    s - name and type of the function argument. 
          --        The name is for debugging purposes only.
          --    t - the return type of the function
          --    f - the haskell representation of the function.
          | LamEH Sig Type (ExpH -> ExpH)

          -- | Conditional expressions.
          | IfEH Type ExpH ExpH ExpH

          -- | Explicit _|_
          | ErrorEH Type String

data ExpH = ExpH {
    eid :: EID,
    force :: ExpH_Value
}

integerEH :: Integer -> ExpH
integerEH = litEH . integerL 

de_integerEH :: ExpH -> Maybe Integer
de_integerEH e = do
    l <- de_litEH e
    de_integerL l

litEH :: Lit -> ExpH
litEH = exph . LitEH

de_litEH :: ExpH -> Maybe Lit
de_litEH e
 | LitEH l <- force e = Just l
 | otherwise = Nothing


instance Show ExpH where
    show = show . force

instance Show ExpH_Value where
    show (LitEH l) = pretty l
    show (ConEH n _ xs) = pretty n ++ " " ++ show xs
    show (VarEH s) = pretty s
    show (PrimEH n _ _ xs) = pretty n ++ " " ++ show xs
    show (LamEH s _ _) = "\\" ++ pretty s ++ " -> ..."
    show (IfEH _ p a b) = "if " ++ show p ++ " then " ++ show a ++ " else " ++ show b
    show (ErrorEH _ s) = "error " ++ show s

trueEH :: ExpH
trueEH = conEH (Sig trueN boolT)

falseEH :: ExpH
falseEH = conEH (Sig falseN boolT)

conEH :: Sig -> ExpH
conEH (Sig n t) =
 let coneh :: Name -> Type -> [ExpH] -> ExpH
     coneh n t args
        | Just (it, ot) <- de_arrowT t =
            lamEH (Sig (name "c") it) ot $ \x -> coneh n ot (args ++ [x])
        | otherwise = aconEH n t args
 in coneh n t []

-- | Boolean expression
boolEH :: Bool -> ExpH
boolEH True = trueEH
boolEH False = falseEH

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

errorEH :: Type -> String -> ExpH
errorEH t s = exph $ ErrorEH t s
    
exph :: ExpH_Value -> ExpH
exph v =
  let {-# NOINLINE idstore #-}
      idstore :: IORef Integer
      idstore = unsafeDupablePerformIO (newIORef 0)
  in unsafeDupablePerformIO $ do
        x <- readIORef idstore
        writeIORef idstore $! x + 1
        return $ ExpH (EID x) v

lamEH :: Sig -> Type -> (ExpH -> ExpH) -> ExpH
lamEH s t f = exph $ LamEH s t f

aconEH :: Name -> Type -> [ExpH] -> ExpH
aconEH n t args = exph $ ConEH n t args

ifEH :: Type -> ExpH -> ExpH -> ExpH -> ExpH
ifEH t p a b =
  case force p of
    ConEH k _ []
      | k == trueN -> a
      | k == falseN -> b
    ErrorEH _ s -> errorEH t s
    _ -> exph $ IfEH t p a b

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


appsEH :: ExpH -> [ExpH] -> ExpH
appsEH f xs = foldl appEH f xs

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
