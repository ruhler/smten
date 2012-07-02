
module Yices2.Yices2 (
    Context(),
    Yices2.Yices2.init, exit, mkctx, run, check,
    SMTStatus(..),
    getIntegerValue,
    ) where

import System.Posix.IO

import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Foreign.Concurrent as F

import Yices2.FFI
import Yices2.Syntax

data Context = Context (ForeignPtr YContext)

-- | Initialize yices.
init :: IO ()
init = c_yices_init

-- | Exit yices
exit :: IO ()
exit = c_yices_exit


-- | Create a new yices2 context for assertions and such.
mkctx :: IO Context
mkctx = do
    ptr <- c_yices_new_context nullPtr
    fp <- F.newForeignPtr ptr (c_yices_free_context ptr)
    return $! Context fp

-- | Run a single yices command, ignoring the result.
run :: Context -> Command -> IO ()
run _ (DefineType s Nothing) = do
    ty <- c_yices_new_uninterpreted_type
    withCString s $ \str -> c_yices_set_type_name ty str
run _ (DefineType s (Just (NormalTD ty))) = do
    ty' <- ytype ty
    withCString s $ \str -> c_yices_set_type_name ty' str
run _ (DefineType s (Just (ScalarTD nms))) = do
    scalar <- c_yices_new_scalar_type (fromIntegral $ length nms)
    withCString s $ \str -> c_yices_set_type_name scalar str
    let defnm :: (String, Int32) -> IO ()
        defnm (nm, idx) = do
            v <- c_yices_constant scalar idx
            withCString nm $ \str -> c_yices_set_term_name v str
    mapM_ defnm (zip nms [0..])
run _ (Define s ty Nothing) = do
    ty' <- ytype ty
    term <- c_yices_new_uninterpreted_term ty'
    withCString s $ \str -> c_yices_set_term_name term str
run (Context fp) (Assert p) = do
    p' <- yterm p
    withForeignPtr fp $ \yctx -> c_yices_assert_formula yctx p'
run ctx Check = check ctx >> return ()
run _ ShowModel = return ()
run (Context fp) Push = withForeignPtr fp c_yices_push
run (Context fp) Pop = withForeignPtr fp c_yices_pop
run _ cmd = error $ "TODO: run " ++ pretty cmd

check :: Context -> IO SMTStatus
check (Context fp) = do
    st <- withForeignPtr fp $ \yctx -> c_yices_check_context yctx nullPtr
    return (fromYSMTStatus st)

withstderr :: (Ptr CFile -> IO a) -> IO a
withstderr f = do
    cf <- withCString "w" $ \str -> c_fdopen 2 str
    x <- f cf 
    return x

ytype :: Type -> IO YType
ytype t = do
    yt <- withCString (pretty t) $ \str -> c_yices_parse_type str
    if yt < 0
        then do
            withstderr $ \stderr -> c_yices_print_error stderr
            error $ "ytype: " ++ pretty t
        else return yt

yterm :: Expression -> IO YTerm
yterm e = do
    ye <- withCString (pretty e) $ \str -> c_yices_parse_term str
    if ye < 0 
        then do 
            withstderr $ \stderr -> c_yices_print_error stderr
            error $ "yterm: " ++ pretty e
        else return ye

-- | Given the name of a free variable with integer type, return its value.
getIntegerValue :: Context -> String -> IO Integer
getIntegerValue (Context fp) nm = do
    model <- withForeignPtr fp $ \yctx -> c_yices_get_model yctx 1
    x <- alloca $ \ptr -> do
        term <- yterm (varE nm)
        ir <- c_yices_get_int64_value model term ptr
        if ir == 0
            then peek ptr
            else error $ "yices2 get int64 value returned: " ++ show ir
    c_yices_free_model model
    return (toInteger x)

