
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}

-- | HOAS form for Smten Expressions, geared towards high performance
-- elaboration.
module Smten.ExpH.ExpH (
    ExpH_(..), EID, ExpH(), force, eid, thunk, forced,
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
    eid :: EID,
    forced_ :: IORef Bool,
    exph_ :: ExpH_
} deriving (Typeable)

force :: ExpH -> ExpH_
force x = unsafePerformIO $ do
    writeIORef (forced_ x) True
    return $! exph_ x

forced :: ExpH -> Bool
forced x = unsafePerformIO $ readIORef (forced_ x)

-- Call the given function with a globally unique identifier.
thunk :: ExpH_ -> ExpH
thunk e = 
  let {-# NOINLINE idstore #-}
      idstore :: IORef Integer
      idstore = unsafePerformIO (newIORef 0)
  in unsafePerformIO $ do
        x <- readIORef idstore
        writeIORef idstore $! x + 1
        r <- newIORef False
        return $ ExpH (EID x) r e

