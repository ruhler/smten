
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}

-- | HOAS form for Smten Expressions, geared towards high performance
-- elaboration.
module Smten.ExpH.ExpH (
    ExpH_Value(..), EID(), ExpH(), force, eid, exph, forced,
    ) where

import System.IO.Unsafe
import Data.Functor((<$>))
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

          -- | Thunk.
          | ThunkEH ExpH
    deriving (Typeable)

-- ExpH_Cell: an ExpH_Value labeled with an expression ID.
data ExpH_Cell = ExpH_Cell {
    cell_id :: EID,
    cell_val :: ExpH_Value
} deriving (Typeable)

newtype ExpH = ExpH {
    exph_cell :: IORef ExpH_Cell
} deriving (Typeable)

force :: ExpH -> ExpH_Value
force = 
 let force_io :: ExpH -> IO ExpH_Cell
     force_io (ExpH r) = do
        c <- readIORef r
        case cell_val c of
            ThunkEH e -> do
                c' <- force_io e
                writeIORef r c'
                return c'
            _ -> return c
 in cell_val . unsafePerformIO . force_io

cell :: ExpH -> ExpH_Cell
cell x = unsafePerformIO $ readIORef (exph_cell x)

value :: ExpH -> ExpH_Value
value = cell_val . cell

eid :: ExpH -> EID
eid = cell_id . cell

forced :: ExpH -> Bool
forced x
 | ThunkEH {} <- value x = True
 | otherwise = False

exph :: ExpH_Value -> ExpH
exph v = 
  let {-# NOINLINE idstore #-}
      idstore :: IORef Integer
      idstore = unsafePerformIO (newIORef 0)
  in unsafePerformIO $ do
        x <- readIORef idstore
        writeIORef idstore $! x + 1
        ExpH <$> newIORef (ExpH_Cell (EID x) v)

