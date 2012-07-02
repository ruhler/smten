
module Yices2.FFI (
    YContext, YContextConfig, YParam, YType, YTerm,
    SMTStatus(..), toYSMTStatus, fromYSMTStatus,
    c_yices_init,
    c_yices_exit,
    c_yices_bool_type,
    c_yices_int_type,
    c_yices_new_uninterpreted_type,
    c_yices_new_uninterpreted_term,
    c_yices_set_type_name,
    c_yices_set_term_name,
    c_yices_get_term_by_name,
    c_yices_eq,
    c_yices_or2,
    c_yices_xor2,
    c_yices_and2,
    c_yices_new_context,
    c_yices_free_context,
    c_yices_assert_formula,
    c_yices_check_context,
    c_yices_get_model,
    c_yices_free_model,
    c_yices_get_int64_value,
    ) where

import Data.Int

import Foreign
import Foreign.C.String
import Foreign.C.Types

data YContext
data YModel
data YContextConfig
data YParam
type YType = Int32
type YTerm = Int32
type YSMTStatus = CInt

data SMTStatus = 
    STATUS_IDLE
  | STATUS_SEARCHING
  | STATUS_UNKNOWN
  | STATUS_SAT
  | STATUS_UNSAT
  | STATUS_INTERRUPTED
  | STATUS_ERROR
    deriving(Eq, Show)

toYSMTStatus :: SMTStatus -> YSMTStatus
toYSMTStatus STATUS_IDLE = 0
toYSMTStatus STATUS_SEARCHING = 1
toYSMTStatus STATUS_UNKNOWN = 2
toYSMTStatus STATUS_SAT = 3
toYSMTStatus STATUS_UNSAT = 4
toYSMTStatus STATUS_INTERRUPTED = 5
toYSMTStatus STATUS_ERROR = 6

fromYSMTStatus :: YSMTStatus -> SMTStatus
fromYSMTStatus 0 = STATUS_IDLE       
fromYSMTStatus 1 = STATUS_SEARCHING  
fromYSMTStatus 2 = STATUS_UNKNOWN    
fromYSMTStatus 3 = STATUS_SAT        
fromYSMTStatus 4 = STATUS_UNSAT      
fromYSMTStatus 5 = STATUS_INTERRUPTED
fromYSMTStatus 6 = STATUS_ERROR      
 

foreign import ccall unsafe "yices_init"
    c_yices_init :: IO ()

foreign import ccall unsafe "yices_exit"
    c_yices_exit :: IO ()

foreign import ccall unsafe "yices_bool_type"
    c_yices_bool_type :: IO YType

foreign import ccall unsafe "yices_int_type"
    c_yices_int_type :: IO YType

foreign import ccall unsafe "yices_new_uninterpreted_type"
    c_yices_new_uninterpreted_type :: IO YType

foreign import ccall unsafe "yices_new_uninterpreted_term"
    c_yices_new_uninterpreted_term :: YType -> IO YTerm

foreign import ccall unsafe "yices_set_type_name"
    c_yices_set_type_name :: YType -> CString -> IO ()

foreign import ccall unsafe "yices_set_term_name"
    c_yices_set_term_name :: YTerm -> CString -> IO ()

foreign import ccall unsafe "yices_get_term_by_name"
    c_yices_get_term_by_name :: CString -> IO YTerm

foreign import ccall unsafe "yices_eq"
    c_yices_eq :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_or2"
    c_yices_or2 :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_xor2"
    c_yices_xor2 :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_and2"
    c_yices_and2 :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_new_context"
    c_yices_new_context :: Ptr YContextConfig -> IO (Ptr YContext)

foreign import ccall unsafe "yices_free_context"
    c_yices_free_context :: Ptr YContext -> IO ()

foreign import ccall unsafe "yices_assert_formula"
    c_yices_assert_formula :: Ptr YContext -> YTerm -> IO ()

foreign import ccall unsafe "yices_check_context"
    c_yices_check_context :: Ptr YContext -> Ptr YParam -> IO YSMTStatus

foreign import ccall unsafe "yices_get_model"
    c_yices_get_model :: Ptr YContext -> Int32 -> IO (Ptr YModel)

foreign import ccall unsafe "yices_free_model"
    c_yices_free_model :: Ptr YModel -> IO ()

foreign import ccall unsafe "yices_get_int64_value"
    c_yices_get_int64_value :: Ptr YModel -> YTerm -> Ptr Int64 -> IO Int32

