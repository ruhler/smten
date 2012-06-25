
{-# LANGUAGE ForeignFunctionInterface   #-}

module Yices.Yices (
    Context, Result(..), mkContext, runCmds, check,
    module Math.SMT.Yices.Syntax
    )
  where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Foreign.Concurrent as F

import Math.SMT.Yices.Syntax

data YContext
data Context = Context (ForeignPtr YContext)


type YBool = CInt

yFalse, yTrue, yUndef :: YBool
yFalse = -1
yTrue  = 1
yUndef = 0


foreign import ccall unsafe "yices_mk_context"
    c_yices_mk_context  :: IO (Ptr YContext)

foreign import ccall unsafe "yices_del_context"
    c_yices_del_context :: Ptr YContext -> IO ()

foreign import ccall unsafe "yices_parse_command"
    c_yices_parse_command :: Ptr YContext -> CString -> IO Bool

foreign import ccall unsafe "yices_check"
    c_yices_check :: Ptr YContext -> IO YBool

data Result
    = Satisfiable
    | Unsatisfiable
    | Undefined
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

toResult :: YBool -> Result
toResult n
    | n == yFalse = Unsatisfiable
    | n == yTrue  = Satisfiable
    | otherwise   = Undefined

mkContext :: IO Context
mkContext = do
    ptr <- c_yices_mk_context
    fp  <- F.newForeignPtr ptr (c_yices_del_context ptr)
    return $! Context fp

runCmd :: Context -> CmdY -> IO ()
runCmd (Context fp) cmd = do
    worked <- withCString (show cmd) $ \str -> do
          withForeignPtr fp $ \yctx ->
            c_yices_parse_command yctx str
    if worked 
       then return ()
       else fail $ "error sending command to yices" 

runCmds :: Context -> [CmdY] -> IO ()
runCmds ctx = mapM_ (runCmd ctx)

check :: Context -> IO Result
check (Context fp) = do
    res <- withForeignPtr fp c_yices_check
    return (toResult res)

