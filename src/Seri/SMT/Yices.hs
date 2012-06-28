
module Seri.SMT.Yices (RunOptions(..), runYices) where

import Data.Generics
import Data.Char(isUpper)
import Data.List((\\))
import Data.Maybe

import System.IO

import Control.Monad.State
import qualified Yices.Yices as Y

import Seri.Failable
import Seri.Lambda
import Seri.Target.Monomorphic.Monomorphic
import Seri.Target.Elaborate
import Seri.Target.Yices.Yices


data YicesState = YicesState {
    ys_decls :: [Dec],
    ys_ctx :: Y.Context,
    ys_dh :: Handle,
    ys_freeid :: Integer
}

type YicesMonad = StateT YicesState IO

sendCmds :: [Y.CmdY] -> Y.Context -> Handle -> IO ()
sendCmds cmds ctx dh = do
    hPutStr dh (unlines (map show cmds))
    Y.runCmds ctx cmds

runCmds :: [Y.CmdY] -> YicesMonad ()
runCmds cmds = do
    ctx <- gets ys_ctx
    dh <- gets ys_dh
    lift $ sendCmds cmds ctx dh

-- Tell yices about any types or expressions needed to refer to the given
-- expression, and return the monomorphized expression.
declareNeeded :: (Monomorphic a, Ppr a) => Env -> a -> YicesMonad a
declareNeeded env x = do
  decs <- gets ys_decls
  let (mds, me) = monomorphic env x
  let (pdecls, _) = sort mds
  let newdecls = pdecls \\ decs
  modify $ \ys -> ys { ys_decls = decs ++ newdecls }
  runCmds (yDecs newdecls)
  return me

freename :: Integer -> String
freename id = yicesN $ "free~" ++ show id

runQuery :: Rule YicesMonad -> Env -> Exp -> YicesMonad Exp
runQuery gr env e = do
    elaborated <- elaborate gr env e
    case elaborated of
        (AppE (VarE (Sig "query" _)) arg) -> do
            dh <- gets ys_dh
            ctx <- gets ys_ctx
            res <- lift $ Y.check ctx
            lift $ hPutStrLn dh $ "; check returned: " ++ show res 
            -- TODO: interpret the evidence from the model.
            case res of 
                Y.Undefined -> return $ ConE (Sig "Unknown" (AppT (ConT "Answer") (typeof arg)))
                Y.Satisfiable -> return $ AppE (ConE (Sig "Satisfiable" (AppT (ConT "Answer") (typeof arg)))) arg
                _ -> return $ ConE (Sig "Unsatisfiable" (AppT (ConT "Answer") (typeof arg)))
        (VarE (Sig "free" (AppT (ConT "Query") t))) -> do
            fid <- gets ys_freeid
            modify $ \ys -> ys {ys_freeid = fid+1}
            
            t' <- declareNeeded env t
            runCmds [Y.DEFINE (freename fid, yType t') Nothing]
            return (VarE (Sig (freename fid) t))
        (AppE (VarE (Sig "assert" _)) p) -> do
            p' <- declareNeeded env p
            runCmds [Y.ASSERT (yExp trueE Y.:= yExp p')]
            return (ConE (Sig "()" (ConT "()")))
        (AppE (VarE (Sig "queryS" _)) q) -> do
            odecls <- gets ys_decls
            runCmds [Y.PUSH]
            x <- runQuery gr env q
            let q' = AppE (VarE (Sig "query" undefined)) x
            y <- runQuery gr env q'
            runCmds [Y.POP]
            modify $ \ys -> ys { ys_decls = odecls }
            return y
        (AppE (VarE (Sig "return_query" _)) x) -> return x
        (AppE (AppE (VarE (Sig "bind_query" _)) x) f) -> do
          result <- runQuery gr env x
          runQuery gr env (AppE f result)
        (AppE (AppE (VarE (Sig "nobind_query" _)) x) y) -> do
          runQuery gr env x
          runQuery gr env y
        x -> error $ "unknown Query: " ++ pretty x


yType :: Type -> Y.TypY
yType t = surely $ yicesT t

yDecs :: [Dec] -> [Y.CmdY]
yDecs ds = surely $ do
    cmds <- mapM yicesD ds
    return $ concat cmds

yExp :: Exp -> Y.ExpY
yExp e = surely $ yicesE e

data RunOptions = RunOptions {
    debugout :: Maybe FilePath
} deriving(Show)
            
runYices :: [Y.CmdY] -> Rule YicesMonad -> RunOptions -> Env -> Exp -> IO Exp
runYices primlib gr opts env e = do
    dh <- openFile (fromMaybe "/dev/null" (debugout opts)) WriteMode

    -- We set NoBuffering, because otherwise the output gets chopped off for
    -- longer outputs.
    hSetBuffering dh NoBuffering
    ctx <- Y.mkContext

    hPutStrLn dh "; Primitives library:"
    sendCmds primlib ctx dh

    -- Declare all possibly needed data type definitions here.
    -- This is to work around a bug in yices when declaring data types inside
    -- a push/pop pair.
    let mds = filter isDataD (fst $ monomorphic env e)
    let pds = fst $ sort mds
    hPutStrLn dh "\n; Data type definitions: "
    sendCmds (yDecs pds) ctx dh

    hPutStrLn dh "\n; Query: "
    (x, _) <- runStateT (runQuery gr env e) (YicesState pds ctx dh 1)
    hClose dh
    return x
    
