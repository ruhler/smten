
-- | An interface to yices2.
module Yices2.Yices2 (
    Context(),
    Yices2.Yices2.init, exit, mkctx, run,
    SMTStatus(..), check,
    getIntegerValue,
    ) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Yices2.FFI
import Yices2.Syntax


debug :: String -> IO ()
debug = putStrLn

data Context = Context (Ptr YContext)

-- | Initialize yices.
init :: IO ()
init = c_yices_init

-- | Exit yices
exit :: IO ()
exit = c_yices_exit


-- | Create a new yices2 context for assertions and such.
-- TODO: this currently leaks context pointers!
-- That should most certainly be fixed somehow.
mkctx :: IO Context
mkctx = do
    debug "mkctx"
    ptr <- c_yices_new_context nullPtr
    return $! Context ptr

-- | Run a single yices command, ignoring the result.
run :: Context -> Command -> IO ()
run _ (DefineType s Nothing) = do
    debug "freetype"
    ty <- c_yices_new_uninterpreted_type
    withCString s $ \str -> c_yices_set_type_name ty str
run _ (DefineType s (Just (NormalTD ty))) = do
    debug "deftype"
    ty' <- ytype ty
    debug "ytype done, setting type name..."
    withCString s $ \str -> c_yices_set_type_name ty' str
    debug "name set"
run _ (DefineType s (Just (ScalarTD nms))) = do
    debug "defscalar"
    scalar <- c_yices_new_scalar_type (fromIntegral $ length nms)
    withCString s $ \str -> c_yices_set_type_name scalar str
    let defnm :: (String, Int32) -> IO ()
        defnm (nm, idx) = do
            v <- c_yices_constant scalar idx
            withCString nm $ \str -> c_yices_set_term_name v str
    mapM_ defnm (zip nms [0..])
run _ (Define s ty Nothing) = do
    debug "free"
    ty' <- ytype ty
    debug "ytype done, making new uniterp term..."
    term <- c_yices_new_uninterpreted_term ty'
    withCString s $ \str -> c_yices_set_term_name term str
    debug "uniterp term made"
run (Context yctx) (Assert p) = do
    debug "assert"
    p' <- yterm p
    c_yices_assert_formula yctx p'
run ctx Check = check ctx >> return ()
run _ ShowModel = do
    debug "show model"
    return ()
run (Context yctx) Push = do
    debug "push"
    c_yices_push yctx
run (Context yctx) Pop = do
    debug "pop"
    c_yices_pop yctx
run _ cmd = error $ "TODO: run " ++ pretty cmd

-- | Run (check) in the given context and return the resulting yices2 status.
check :: Context -> IO SMTStatus
check (Context yctx) = do
    debug "check"
    st <- c_yices_check_context yctx nullPtr
    return $! fromYSMTStatus st

withstderr :: (Ptr CFile -> IO a) -> IO a
withstderr f = do
    debug "witherr"
    cf <- withCString "w" $ \str -> c_fdopen 2 str
    x <- f cf 
    return $! x

ytype :: Type -> IO YType
ytype t = do
    debug "ytype"
    yt <- withCString (pretty t) $ \str -> c_yices_parse_type str
    debug $ "ytype got: " ++ show yt
    if yt < 0
        then do
            debug "yt < 0"
            withstderr $ \stderr -> c_yices_print_error stderr
            error $ "ytype: " ++ pretty t
        else do
            debug "yt ok"
            return $! yt

yterm :: Expression -> IO YTerm
yterm e = do
    debug "yterm"
    ye <- withCString (pretty e) $ \str -> c_yices_parse_term str
    if ye < 0 
        then do 
            withstderr $ \stderr -> c_yices_print_error stderr
            error $ "yterm error"
        else return $! ye

-- | Given the name of a free variable with integer type, return its value.
getIntegerValue :: Context -> String -> IO Integer
getIntegerValue (Context yctx) nm = do
    debug "getIntval"
    model <- c_yices_get_model yctx 1
    x <- alloca $ \ptr -> do
            term <- yterm (varE nm)
            ir <- c_yices_get_int64_value model term ptr
            if ir == 0
               then do 
                  v <- peek ptr
                  return $! v
               else error $ "yices2 get int64 value returned: " ++ show ir
    c_yices_free_model model
    return $! toInteger x

