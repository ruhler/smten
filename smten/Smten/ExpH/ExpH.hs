
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}

-- | HOAS form for Smten Expressions, geared towards high performance
-- elaboration.
module Smten.ExpH.ExpH (
    ExpH_(..), EID, ExpH(), force, eid, thunk, thunkNS, identify,
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

-- ExpH_ represents a symbolic Smten expression evaluated to normal form. 
data ExpH_ =
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
    deriving(Typeable)

data ExpH = ExpH {
    eid :: Maybe EID,
    force :: ExpH_
} deriving (Typeable)

-- Call the given function with a globally unique identifier.
thunk :: ExpH_ -> ExpH
thunk e = identify $ \x -> ExpH (Just x) e

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
thunkNS :: ExpH_ -> ExpH
thunkNS = ExpH Nothing

