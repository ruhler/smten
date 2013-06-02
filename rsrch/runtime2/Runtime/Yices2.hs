
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}

-- | Backend for the Yices2 solver
module Runtime.Yices2 (
    Solver, Result(..), yices2, assert, getBoolValue, check,
    fresh_bool,
    ) where

import Foreign
import Foreign.C.String

import Data.IORef

import qualified Runtime.Prelude as R
import Smten.SMT.Solver(Result(..))
import Smten.SMT.Yices.FFI2
import Smten.Name

data Solver = Solver {
    yctx :: Ptr YContext,
    ynid :: IORef Integer
}

-- TODO: this currently leaks context pointers!
-- That should most certainly be fixed somehow.
-- TODO: when do we call c_yices_exit?
yices2 :: IO Solver
yices2 = do
  c_yices_init
  ctx <- c_yices_new_context nullPtr
  nid <- newIORef 0
  return $ Solver ctx nid

assert :: Solver -> R.Bool -> IO ()
assert y p = do
    p' <- mkterm p
    c_yices_assert_formula (yctx y) p'

check :: Solver -> IO Result
check y = do
    st <- c_yices_check_context (yctx y) nullPtr
    return $! fromYSMTStatus st

getBoolValue :: Solver -> Name -> IO R.Bool
getBoolValue y nm = do
    model <- c_yices_get_model (yctx y) 1
    x <- alloca $ \ptr -> do
            term <- withCString (unname nm) c_yices_get_term_by_name
            ir <- c_yices_get_bool_value model term ptr
            case ir of
               _ | ir == (-1) -> do
                  -- -1 means we don't care, so just return the equivalent
                  -- of False.
                  return 0

               0 -> do 
                  v <- peek ptr
                  return v

               _ -> error $ "yices2 get bool value returned: " ++ show ir
    c_yices_free_model model
    case x of
        0 -> return R.False
        1 -> return R.True
        _ -> error $ "yices2 get bool value got: " ++ show x

mkterm :: R.Bool -> IO YTerm
mkterm R.True = c_yices_true
mkterm R.False = c_yices_false
mkterm (R.BoolVar nm) = withCString (unname nm) c_yices_get_term_by_name
mkterm (R.BoolMux p a b) = do
    p' <- mkterm p
    a' <- mkterm a
    b' <- mkterm b
    c_yices_ite p' a' b'

fresh_bool :: Solver -> IO Name
fresh_bool y = do
    nid <- readIORef (ynid y)
    modifyIORef' (ynid y) (+ 1)
    let nm = "f~" ++ show nid
    ty <- c_yices_bool_type
    term <- c_yices_new_uninterpreted_term ty   
    withCString nm $ c_yices_set_term_name term
    return $ name nm

