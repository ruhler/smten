
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}

-- | HOAS form for Smten Expressions, geared towards high performance
-- elaboration.
module Smten.ExpH.ExpH (
    ExpH(..), EID, Thunk(), force, eid, thunk, thunkNS, identify,
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
          | ConEH Name Type [Thunk]

            -- | Primitive symbolic varibles.
            -- Current types supported are: Bool, Integer, Bit
          | VarEH Sig

            -- | Fully applied primitive functions
            -- | The Type field is the type of the fully applied primitive.
          | PrimEH Name Type ([Thunk] -> Thunk) [Thunk]
         
          -- | LamEH s t f:
          --    s - name and type of the function argument. 
          --        The name is for debugging purposes only.
          --    t - the return type of the function
          --    f - the haskell representation of the function.
          | LamEH Sig Type (Thunk -> Thunk)

          -- | Conditional expressions.
          | IfEH Type Thunk Thunk Thunk
    deriving(Typeable)

data Thunk = Thunk {
    eid :: Maybe EID,
    force :: ExpH
} deriving (Typeable)

-- Call the given function with a globally unique identifier.
thunk :: ExpH -> Thunk
thunk e = identify $ \x -> Thunk (Just x) e

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

-- Create a non-sharing thunk.
-- Use this for things like literals which there is no point in sharing.
thunkNS :: ExpH -> Thunk
thunkNS = Thunk Nothing

