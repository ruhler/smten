
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}

-- | HOAS form for Smten Expressions, geared towards high performance
-- elaboration.
module Smten.ExpH.ExpH (
    ExpH_Value(..), EID(), ExpH(), force, eid, exph, simple,
    ) where

import System.IO.Unsafe
import Data.Functor((<$>))
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Typeable
import Data.Hashable

import Smten.Lit
import Smten.Name
import Smten.Type
import Smten.Sig
import Smten.Ppr
import Smten.Strict

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
    deriving (Typeable)

data ExpH = ExpH {
    eid :: EID,
    force :: ExpH_Value
} deriving (Typeable)

instance Show ExpH where
    show = show . force

instance Show ExpH_Value where
    show (LitEH l) = pretty l
    show (ConEH n _ xs) = pretty n ++ " " ++ show xs
    show (VarEH s) = pretty s
    show (PrimEH n _ _ xs) = pretty n ++ " " ++ show xs
    show (LamEH s _ _) = "\\" ++ pretty s ++ " -> ..."
    show (IfEH _ p a b) = "if " ++ show p ++ " then " ++ show a ++ " else " ++ show b
    
exph :: ExpH_Value -> ExpH
exph v = 
  let {-# NOINLINE idstore #-}
      idstore :: IORef Integer
      idstore = unsafePerformIO (newIORef 0)
  in unsafePerformIO $ do
        x <- readIORef idstore
        writeIORef idstore $! x + 1
        return $ ExpH (EID x) v

-- Return true if the given expression is simple.
-- Note: Error is not considered simple, because we want ABSTRACT to share the
-- monadic abstraction.
--
-- TODO: That seems a bit hackish to have to know about here...
simple :: ExpH -> Bool
simple e =
  case force e of
     LitEH {} -> True
     ConEH _ _ [] -> True
     VarEH {} -> True
     _ -> False

