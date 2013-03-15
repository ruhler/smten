
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}

-- | HOAS form for Smten Expressions, geared towards high performance
-- elaboration.
module Smten.ExpH.ExpH (
    ExpH(..), EID, identify, getid,
    ) where

import System.IO.Unsafe
import Data.IORef
import Data.Typeable
import Data.Hashable

import Smten.Lit
import Smten.Name
import Smten.Type
import Smten.Sig
import Smten.Strict

newtype EID = EID Integer
    deriving (Eq, Hashable, Ord)

instance Show EID where
    show (EID x) = show x

-- ExpH represents a symbolic Smten expression evaluated to normal form. 
data ExpH =
            -- | Literal characters and integers
            LitEH Lit

            -- | Fully applied data constructors.
            -- The Type field is the type of the fully applied constructor.
          | ConEH EID Name Type [ExpH]

            -- | Primitive symbolic varibles.
            -- Current types supported are: Bool, Integer, Bit
          | VarEH Sig

            -- | Fully applied primitive functions
            -- | The Type field is the type of the fully applied primitive.
          | PrimEH EID Name Type ([ExpH] -> ExpH) [ExpH]
         
          -- | LamEH s t f:
          --    s - name and type of the function argument. 
          --        The name is for debugging purposes only.
          --    t - the return type of the function
          --    f - the haskell representation of the function.
          | LamEH EID Sig Type (ExpH -> ExpH)

          -- | Conditional expressions.
          | IfEH EID ExpH ExpH ExpH

          -- | Explicit _|_.
          -- Type is the type of the expression.
          --
          -- TODO: Ideally we shouldn't need this at all, because error and
          -- non-termination should be indistinguishable, so we could just
          -- convert to a haskell error immediately. Our current
          -- implementation, however, is overly eager when things are
          -- symbolic. Having explicit error lets us at least handle that case
          -- more nicely. (Though it doesn't work for non-termination).
          | ErrorEH Type String
    deriving(Typeable)

-- Call the given function with a globally unique identifier.
identify :: (EID -> a) -> a
identify f = 
  let {-# NOINLINE idstore #-}
      idstore :: IORef Integer
      idstore = unsafePerformIO (newIORef 0)

      identifyIO :: (EID -> a) -> IO a
      identifyIO f = do
        x <- readIORef idstore
        writeIORef idstore $! x + 1
        return $! (f $! EID x)
  in unsafePerformIO $ identifyIO f

-- Return the EID of the given complex expression, or None if the
-- expression is simple
getid :: ExpH -> Maybe EID
getid e
  | ConEH _ _ _ [] <- e = Nothing
  | ConEH x _ _ _ <- e = Just x
  | PrimEH x _ _ _ _ <- e = Just x
  | LamEH x _ _ _ <- e = Just x
  | IfEH x _ _ _ <- e = Just x
  | otherwise = Nothing

