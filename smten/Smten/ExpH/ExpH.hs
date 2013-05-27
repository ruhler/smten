
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}

-- | HOAS form for Smten Expressions, geared towards high performance
-- elaboration.
module Smten.ExpH.ExpH (
    ExpH_Value(..), EID(), ExpH(), typeof, force, eid, exph, simple,
    ) where

import System.IO.Unsafe
import Data.IORef
import Data.Typeable
import Data.Hashable

import Smten.Lit
import Smten.Name
import Smten.Type
import Smten.Ppr

newtype EID = EID Integer
    deriving (Eq, Hashable, Ord)

instance Show EID where
    show (EID x) = show x

-- ExpH_Value represents a symbolic expression evaluated to weak head normal
-- form, or a thunk.
data ExpH_Value =
            -- | Literal characters and integers
            LitEH Lit

            -- | Fully applied data constructors.
          | ConEH Name [ExpH]

            -- | Primitive symbolic varibles.
            -- Current types supported are: Bool, Integer, Bit
          | VarEH Name

            -- | Fully applied primitive functions
          | PrimEH Name ([ExpH] -> ExpH) [ExpH]
         
          -- | lambda expression
          | LamEH (ExpH -> ExpH)

          -- | Conditional expressions.
          | IfEH ExpH ExpH ExpH

          -- | Explicit _|_
          | ErrorEH String
    deriving (Typeable)

data ExpH = ExpH {
    eid :: EID,
    typeof_ :: Type,
    force :: ExpH_Value
} deriving (Typeable)

instance Show ExpH where
    show = show . force

instance Show ExpH_Value where
    show (LitEH l) = pretty l
    show (ConEH n xs) = pretty n ++ " " ++ show xs
    show (VarEH n) = pretty n
    show (PrimEH n _ xs) = pretty n ++ " " ++ show xs
    show (LamEH _) = "\\? -> ..."
    show (IfEH p a b) = "if " ++ show p ++ " then " ++ show a ++ " else " ++ show b
    show (ErrorEH s) = "error " ++ show s

{-# NOINLINE idstore #-}
idstore :: IORef Integer
idstore = unsafePerformIO (newIORef 0)

mkeid :: Type -> (EID, Type)
mkeid t = {-# SCC "mkeid" #-} unsafeDupablePerformIO $ do
   x <- readIORef idstore
   writeIORef idstore $! x + 1
   return $ (EID x, t)
    
exph :: Type -> ExpH_Value -> ExpH
exph t v = ExpH (fst (mkeid t)) t v

-- Return true if the given expression is simple.
simple :: ExpH -> Bool
simple e =
  case force e of
     LitEH {} -> True
     ConEH _ [] -> True
     VarEH {} -> True
     ErrorEH {} -> True
     _ -> False

instance Typeof ExpH where
    typeof = typeof_

