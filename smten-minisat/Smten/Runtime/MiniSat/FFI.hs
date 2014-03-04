
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | FFI interface to minisat.
module Smten.Runtime.MiniSat.FFI (
    MSSolver, MSExpr, 
    c_minisat_new, c_minisat_delete,
    c_minisat_var, c_minisat_getvar,
    c_minisat_check, c_minisat_assert, 
    c_minisat_true, c_minisat_false,
    c_minisat_not, c_minisat_and, c_minisat_or, c_minisat_ite,
    ) where

import Foreign
import Foreign.C.Types

data MSSolver_
type MSSolver = Ptr MSSolver_

type MSExpr = CInt

foreign import ccall unsafe "minisat_new"
    c_minisat_new :: IO MSSolver

foreign import ccall unsafe "minisat_delete"
    c_minisat_delete :: MSSolver -> IO ()

foreign import ccall unsafe "minisat_var"
    c_minisat_var :: MSSolver -> IO MSExpr

foreign import ccall unsafe "minisat_getvar"
    c_minisat_getvar :: MSSolver -> MSExpr -> IO CInt

foreign import ccall unsafe "minisat_check"
    c_minisat_check :: MSSolver -> IO CInt

foreign import ccall unsafe "minisat_assert"
    c_minisat_assert :: MSSolver -> MSExpr -> IO ()

foreign import ccall unsafe "minisat_true"
    c_minisat_true :: MSSolver -> IO MSExpr

foreign import ccall unsafe "minisat_false"
    c_minisat_false :: MSSolver -> IO MSExpr

foreign import ccall unsafe "minisat_not"
    c_minisat_not :: MSSolver -> MSExpr -> IO MSExpr

foreign import ccall unsafe "minisat_and"
    c_minisat_and :: MSSolver -> MSExpr -> MSExpr -> IO MSExpr

foreign import ccall unsafe "minisat_or"
    c_minisat_or :: MSSolver -> MSExpr -> MSExpr -> IO MSExpr

foreign import ccall unsafe "minisat_ite"
    c_minisat_ite :: MSSolver -> MSExpr -> MSExpr -> MSExpr -> IO MSExpr
