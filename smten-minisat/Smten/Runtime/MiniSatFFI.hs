
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | FFI interface to minisat.
module Smten.Runtime.MiniSatFFI (
    MSSolver, MSVar, 
    c_minisat_mksolver, c_minisat_delsolver,
    c_minisat_mkvar, c_minisat_getvar,
    c_minisat_issat,
    c_minisat_addclause1,
    c_minisat_addclause2,
    c_minisat_addclause3,
    ) where

import Foreign
import Foreign.C.Types

data MSSolver_
type MSSolver = Ptr MSSolver_

type MSVar = CInt

foreign import ccall unsafe "minisat_mksolver"
    c_minisat_mksolver :: IO MSSolver

foreign import ccall unsafe "minisat_delsolver"
    c_minisat_delsolver :: MSSolver -> IO ()

foreign import ccall unsafe "minisat_mkvar"
    c_minisat_mkvar :: MSSolver -> IO MSVar

foreign import ccall unsafe "minisat_getvar"
    c_minisat_getvar :: MSSolver -> MSVar -> IO CInt

foreign import ccall unsafe "minisat_issat"
    c_minisat_issat :: MSSolver -> IO Bool

foreign import ccall unsafe "minisat_addclause1"
    c_minisat_addclause1 :: MSSolver -> MSVar -> Bool -> IO ()

foreign import ccall unsafe "minisat_addclause2"
    c_minisat_addclause2 :: MSSolver -> MSVar -> Bool
                                     -> MSVar -> Bool -> IO ()

foreign import ccall unsafe "minisat_addclause3"
    c_minisat_addclause3 :: MSSolver -> MSVar -> Bool
                                     -> MSVar -> Bool
                                     -> MSVar -> Bool -> IO ()
