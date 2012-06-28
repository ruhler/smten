
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls  #-}

-- | FFI Interface to yices 1.
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
data YModel
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

foreign import ccall unsafe "yices_get_model"
    c_yices_get_model :: Ptr YContext -> IO (Ptr YModel)

foreign import ccall unsafe "yices_display_model"
    c_yices_display_model :: Ptr YModel -> IO ()

foreign import ccall unsafe "yices_enable_type_checker"
    c_yices_enable_type_checker :: Bool -> IO ()

foreign import ccall unsafe "yices_get_last_error_message"
    c_yices_get_last_error_message :: IO CString

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

-- | Initialize a new context for interacting with yices.
mkContext :: IO Context
mkContext = do
    c_yices_enable_type_checker True
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
       else do
          cstr <- c_yices_get_last_error_message
          msg <- peekCString cstr
          fail $ show msg
                    ++ "\n when running command: \n" 
                    ++ show cmd

-- | Send a bunch of commands to yices.
runCmds :: Context -> [CmdY] -> IO ()
runCmds ctx = mapM_ (runCmd ctx)

-- | Send a check command to yices and interpret the result.
check :: Context -> IO Result
check (Context fp) = do
    res <- withForeignPtr fp c_yices_check
    let result = toResult res
    case result of
        Satisfiable -> do
           -- Print out the model for debugging.
           -- TODO: it would be nice if we could read the model in a
           -- meaningful way. I haven't figured out how to do that yet though.
           model <- withForeignPtr fp c_yices_get_model
           c_yices_display_model model
        _ -> return ()
    return result

