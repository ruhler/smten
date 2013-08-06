
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}

-- | An interface to the yices2 library which logs calls in the form
-- of c-code to make it easy to reproduce any issues that arise with using
-- yices2.
module Smten.Runtime.Yices2FFICGen (
    YContext, YModel, YType, YTerm, Y.fromYSMTStatus,
    c_yices_init,
    c_yices_exit,
    c_yices_new_uninterpreted_term,
    c_yices_set_term_name,
    c_yices_get_term_by_name,
    c_yices_bv_type,
    c_yices_int_type,
    c_yices_bool_type,
    
    c_yices_true,
    c_yices_false,
    c_yices_int64,
    c_yices_not,
    c_yices_eq,
    c_yices_ite,
    c_yices_arith_leq_atom,
    c_yices_add,
    c_yices_sub,
    c_yices_mul,
    c_yices_and2,

    c_yices_bvconst_uint64,
    c_yices_parse_bvbin,
    c_yices_bvadd,
    c_yices_bvsub,
    c_yices_bvmul,
    c_yices_bvand,
    c_yices_bvor,
    c_yices_bvnot,
    c_yices_zero_extend,
    c_yices_sign_extend,
    c_yices_bvshl,
    c_yices_bvlshr,
    c_yices_bvextract,
    c_yices_bvconcat,
    c_yices_new_context,
    c_yices_free_context,
    c_yices_assert_formula,
    c_yices_check_context,
    c_yices_get_model,
    c_yices_free_model,
    c_yices_get_bool_value,
    c_yices_get_int64_value,
    c_yices_get_bv_value,
    c_yices_bvle_atom,
 ) where

import Data.Int
import Data.IORef

import Foreign
import Foreign.C.String

import qualified System.IO.Unsafe as S

import qualified Smten.Runtime.Yices2FFI as Y

type ID = Integer

type YContext = Y.YContext
type YModel = Y.YModel

data YType = YType {
  yty_id :: ID,
  yty_v :: Y.YType
}

data YTerm = YTerm {
  ytm_id :: ID,
  ytm_v :: Y.YTerm
}

typeid :: IORef ID
typeid = S.unsafePerformIO $ newIORef 0

ntype :: IO Y.YType -> IO YType
ntype x = do
    v <- x
    i <- readIORef typeid
    modifyIORef' typeid (+ 1)
    return (YType i v)

termid :: IORef ID
termid = S.unsafePerformIO $ newIORef 0

nterm :: IO Y.YTerm -> IO YTerm
nterm x = do
    v <- x
    i <- readIORef termid
    modifyIORef' termid (+ 1)
    return (YTerm i v)

class Pr a where
    pr :: a -> String
    prr :: a -> String

instance Pr YType where
    pr x = "t" ++ show (yty_id x)
    prr x = "type_t " ++ pr x

instance Pr YTerm where
    pr x = "e" ++ show (ytm_id x)
    prr x = "term_t " ++ pr x

instance Pr Int32 where
    pr = show
    prr x = "int32_t " ++ pr x

instance Pr (Ptr Y.YContextConfig) where
    pr x = "p" ++ show x
    prr x = "ctx_config_t*" ++ pr x

instance Pr (Ptr Y.YParam) where
    pr x = "p" ++ show x
    prr x = "param_t*" ++ pr x

instance Pr (Ptr Y.YContext) where
    pr x = "p" ++ show x
    prr x = "context_t*" ++ pr x

instance Pr (Ptr Int32) where
    pr x = "p" ++ show x
    prr x = "int32_t*" ++ pr x

instance Pr (Ptr Int64) where
    pr x = "p" ++ show x
    prr x = "int64_t*" ++ pr x

instance Pr (Ptr Y.YModel) where
    pr x = "p" ++ show x
    prr x = "model_t*" ++ pr x


y1 :: String -> (Y.YTerm -> IO Y.YTerm) -> YTerm -> IO YTerm
y1 nm f x = do
    r <- nterm $ f (ytm_v x)
    putStrLn $ prr r ++ " = " ++ nm ++ "(" ++ pr x ++ ");"
    return r

y2 :: String -> (Y.YTerm -> Y.YTerm -> IO Y.YTerm) -> YTerm -> YTerm -> IO YTerm
y2 nm f x y = do
    r <- nterm $ f (ytm_v x) (ytm_v y)
    putStrLn $ prr r ++ " = " ++ nm ++ "(" ++ pr x ++ ", " ++ pr y ++ ");"
    return r

y3 :: String -> (Y.YTerm -> Y.YTerm -> Y.YTerm -> IO Y.YTerm)
             -> YTerm -> YTerm -> YTerm -> IO YTerm
y3 nm f x y z = do
    r <- nterm $ f (ytm_v x) (ytm_v y) (ytm_v z)
    putStrLn $ prr r ++ " = " ++ nm ++ "(" ++ pr x ++ ", "
                                          ++ pr y ++ ", "
                                          ++ pr z ++ ");"
    return r

c_yices_init :: IO ()
c_yices_init = do
    putStrLn "yices_init();"
    Y.c_yices_init

c_yices_exit :: IO ()
c_yices_exit = do
    putStrLn "yices_exit();"
    Y.c_yices_exit

c_yices_new_uninterpreted_term :: YType -> IO YTerm
c_yices_new_uninterpreted_term x = do
    r <- nterm $ Y.c_yices_new_uninterpreted_term (yty_v x)
    putStrLn $ prr r ++ " = " ++ "yices_new_uninterpreted_term("
                             ++ pr x ++ ");"
    return r

c_yices_set_term_name :: YTerm -> CString -> IO ()
c_yices_set_term_name a b = do
    Y.c_yices_set_term_name (ytm_v a) b
    b' <- peekCString b
    putStrLn $ "yices_set_term_name(" ++ pr a ++ ", " ++ show b' ++ ");"

c_yices_get_term_by_name :: CString -> IO YTerm
c_yices_get_term_by_name a = do
    r <- nterm $ Y.c_yices_get_term_by_name a
    a' <- peekCString a
    putStrLn $ prr r ++ " = yices_get_term_by_name(" ++ show a' ++ ");"
    return r

c_yices_bv_type :: Word32 -> IO YType
c_yices_bv_type w = do
    r <- ntype $ Y.c_yices_bv_type w
    putStrLn $ prr r ++ " = yices_bv_type(" ++ show w ++ ");"
    return r
    
c_yices_int_type :: IO YType
c_yices_int_type = do
    r <- ntype $ Y.c_yices_int_type
    putStrLn $ prr r ++ " = yices_int_type();"
    return r

c_yices_bool_type :: IO YType
c_yices_bool_type = do
    r <- ntype $ Y.c_yices_bool_type
    putStrLn $ prr r ++ " = yices_bool_type();"
    return r

c_yices_true ::  IO YTerm
c_yices_true = do
    r <- nterm $ Y.c_yices_true
    putStrLn $ prr r ++ " = yices_true();"
    return r

c_yices_false ::  IO YTerm
c_yices_false = do
    r <- nterm $ Y.c_yices_false
    putStrLn $ prr r ++ " = yices_false();"
    return r

c_yices_int64 :: Int64 ->  IO YTerm
c_yices_int64 x = do
    r <- nterm $ Y.c_yices_int64 x
    putStrLn $ prr r ++ " = yices_int64(" ++ show x ++ ");"
    return r

c_yices_not ::  YTerm -> IO YTerm
c_yices_not = y1 "yices_not" Y.c_yices_not

c_yices_eq :: YTerm -> YTerm -> IO YTerm
c_yices_eq = y2 "yices_eq" Y.c_yices_eq

c_yices_ite :: YTerm -> YTerm -> YTerm -> IO YTerm
c_yices_ite = y3 "yices_ite" Y.c_yices_ite

c_yices_arith_leq_atom :: YTerm -> YTerm -> IO YTerm
c_yices_arith_leq_atom = y2 "yices_arith_leq_atom" Y.c_yices_arith_leq_atom

c_yices_add :: YTerm -> YTerm -> IO YTerm
c_yices_add = y2 "yices_add" Y.c_yices_add

c_yices_sub :: YTerm -> YTerm -> IO YTerm
c_yices_sub = y2 "yices_sub" Y.c_yices_sub

c_yices_mul :: YTerm -> YTerm -> IO YTerm
c_yices_mul = y2 "yices_mul" Y.c_yices_mul

c_yices_and2 :: YTerm -> YTerm -> IO YTerm
c_yices_and2 = y2 "yices_and2" Y.c_yices_and2

c_yices_bvconst_uint64 :: Word32 -> Word64 -> IO YTerm
c_yices_bvconst_uint64 x y = do
    r <- nterm $ Y.c_yices_bvconst_uint64 x y
    putStrLn $ prr r ++ " = yices_bvconst_uint64(" ++ show x ++ ", " ++ show y ++ ");"
    return r

c_yices_parse_bvbin :: CString -> IO YTerm
c_yices_parse_bvbin x = do
    r <- nterm $ Y.c_yices_parse_bvbin x
    x' <- peekCString x
    putStrLn $ prr r ++ " = yices_parse_bvbin(" ++ show x' ++ ");"
    return r

c_yices_bvadd :: YTerm -> YTerm -> IO YTerm
c_yices_bvadd = y2 "yices_bvadd" Y.c_yices_bvadd

c_yices_bvsub :: YTerm -> YTerm -> IO YTerm
c_yices_bvsub = y2 "yices_bvsub" Y.c_yices_bvsub

c_yices_bvmul :: YTerm -> YTerm -> IO YTerm
c_yices_bvmul = y2 "yices_bvmul" Y.c_yices_bvmul

c_yices_bvand :: YTerm -> YTerm -> IO YTerm
c_yices_bvand = y2 "yices_bvand" Y.c_yices_bvand

c_yices_bvor :: YTerm -> YTerm -> IO YTerm
c_yices_bvor = y2 "yices_bvor" Y.c_yices_bvor

c_yices_bvnot :: YTerm -> IO YTerm
c_yices_bvnot = y1 "yices_bvnot" Y.c_yices_bvnot

c_yices_zero_extend :: YTerm -> Word32 -> IO YTerm
c_yices_zero_extend x y = do
    r <- nterm $ Y.c_yices_zero_extend (ytm_v x) y
    putStrLn $ prr r ++ " = yices_zero_extend(" ++ pr x ++ ", " ++ show y ++ ");"
    return r

c_yices_sign_extend :: YTerm -> Word32 -> IO YTerm
c_yices_sign_extend x y = do
    r <- nterm $ Y.c_yices_sign_extend (ytm_v x) y
    putStrLn $ prr r ++ " = yices_sign_extend(" ++ pr x ++ ", " ++ show y ++ ");"
    return r

c_yices_bvshl :: YTerm -> YTerm -> IO YTerm
c_yices_bvshl = y2 "yices_bvshl" Y.c_yices_bvshl

c_yices_bvlshr :: YTerm -> YTerm -> IO YTerm
c_yices_bvlshr = y2 "yices_bvlshr" Y.c_yices_bvlshr

c_yices_bvextract :: YTerm -> Word32 -> Word32 -> IO YTerm
c_yices_bvextract x y z = do
    r <- nterm $ Y.c_yices_bvextract (ytm_v x) y z
    putStrLn $ prr r ++ " = yices_bvextract(" ++ pr x ++ ", "
                                             ++ show y ++ ", "
                                             ++ show z ++ ");"
    return r

c_yices_bvconcat :: YTerm -> YTerm -> IO YTerm
c_yices_bvconcat = y2 "yices_bvconcat" Y.c_yices_bvconcat

c_yices_new_context :: Ptr Y.YContextConfig -> IO (Ptr YContext)
c_yices_new_context x = do
    r <- Y.c_yices_new_context x
    putStrLn $ prr r ++ " = yices_new_context(" ++ pr x ++ ");"
    return r
    
c_yices_free_context :: Ptr YContext -> IO ()
c_yices_free_context x = do
    Y.c_yices_free_context x
    putStrLn $ "yices_free_context(" ++ pr x ++ ");"

c_yices_assert_formula :: Ptr YContext -> YTerm -> IO ()
c_yices_assert_formula x y = do 
    Y.c_yices_assert_formula x (ytm_v y)
    putStrLn $ "yices_assert_formula(" ++ pr x ++ ", " ++ pr y ++ ");"

c_yices_check_context :: Ptr YContext -> Ptr Y.YParam -> IO Y.YSMTStatus
c_yices_check_context x y = do
    r <- Y.c_yices_check_context x y
    putStrLn $ "yices_check_context(" ++ pr x ++ ", " ++ pr y ++ ");"
    return r

c_yices_get_model :: Ptr YContext -> Int32 -> IO (Ptr YModel)
c_yices_get_model x y = do
    r <- Y.c_yices_get_model x y
    putStrLn $ prr r ++ " = yices_get_model(" ++ pr x ++ ", " ++ pr y ++ ");"
    return r

c_yices_free_model :: Ptr YModel -> IO ()
c_yices_free_model x = do
    Y.c_yices_free_model x
    putStrLn $ "yices_free_model(" ++ pr x ++ ");"
    

c_yices_get_bool_value :: Ptr YModel -> YTerm -> Ptr Int32 -> IO Int32
c_yices_get_bool_value x y z = do
    r <- Y.c_yices_get_bool_value x (ytm_v y) z
    putStrLn $ "yices_get_bool_value(" ++ pr x ++ ", " ++ pr y ++ ", " ++ pr z ++ ");"
    return r

c_yices_get_int64_value :: Ptr YModel -> YTerm -> Ptr Int64 -> IO Int32
c_yices_get_int64_value x y z = do
    r <- Y.c_yices_get_int64_value x (ytm_v y) z
    putStrLn $ "yices_get_int64_value(" ++ pr x ++ ", " ++ pr y ++ ", " ++ pr z ++ ");"
    return r

c_yices_get_bv_value :: Ptr YModel -> YTerm -> Ptr Int32 -> IO Int32
c_yices_get_bv_value x y z = do
    r <- Y.c_yices_get_bv_value x (ytm_v y) z
    putStrLn $ "yices_get_bv_value(" ++ pr x ++ ", " ++ pr y ++ ", " ++ pr z ++ ");"
    return r

c_yices_bvle_atom :: YTerm -> YTerm -> IO YTerm
c_yices_bvle_atom = y2 "yices_bvle_atom" Y.c_yices_bvle_atom

