
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

import qualified Data.HashTable.IO as H

import System.Mem.StableName
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
    putStrLn ("assert " ++ showt p')
    c_yices_assert_formula (yctx y) (yterm p')

check :: Solver -> IO Result
check y = do
    st <- c_yices_check_context (yctx y) nullPtr
    return $! fromYSMTStatus st

getBoolValue :: Solver -> Name -> IO R.Bool__
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

data Term = Term {
    yterm :: YTerm,
    dterm :: Integer
}

{-# NOINLINE did #-}
did :: IORef Integer
did = unsafePerformIO (newIORef 0)

newid :: IO Integer
newid = do
    v <- readIORef did
    modifyIORef' did (+ 1)
    return v


type TermCache = H.BasicHashTable (StableName R.Bool) Term

{-# NOINLINE tcache #-}
tcache :: TermCache
tcache = unsafePerformIO H.new

tinsert :: R.Bool -> Term -> IO ()
tinsert k v = do
   nm <- makeStableName $! k
   H.insert tcache nm v

tlookup :: R.Bool -> IO (Maybe Term)
tlookup v = do
   nm <- makeStableName $! v 
   H.lookup tcache nm
    
dbg :: String -> IO YTerm -> IO Term
dbg msg m = do
    v <- m
    id <- newid
    let t = Term v id
    putStrLn $ showt t ++ ": " ++ msg
    return t

showt :: Term -> String
showt x = "$" ++ show (dterm x)

mkterm :: R.Bool -> IO Term
mkterm p = do
    r <- tlookup p
    case r of 
        Just v -> return v
        Nothing -> do
            v <- mkterm' p
            tinsert p v
            return v

mkterm' :: R.Bool -> IO Term
mkterm' (R.Concrete R.True) = dbg "True" c_yices_true
mkterm' (R.Concrete R.False) = dbg "False" c_yices_false
mkterm' (R.Concrete (R.BoolVar nm)) = dbg (unname nm) $
    withCString (unname nm) c_yices_get_term_by_name
mkterm' (R.Mux p a b) = do
    p' <- mkterm p
    a' <- mkterm a
    b' <- mkterm b
    dbg (showt p' ++ " ? " ++ showt a' ++ " : " ++ showt b') $ c_yices_ite (yterm p') (yterm a') (yterm b')

fresh_bool :: Solver -> IO Name
fresh_bool y = do
    nid <- readIORef (ynid y)
    modifyIORef' (ynid y) (+ 1)
    let nm = "f~" ++ show nid
    ty <- c_yices_bool_type
    term <- c_yices_new_uninterpreted_term ty   
    withCString nm $ c_yices_set_term_name term
    return $ name nm

