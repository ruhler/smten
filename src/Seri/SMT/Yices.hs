
module Seri.SMT.Yices (RunOptions(..), runYices) where

import Data.Generics
import Data.Char(isUpper)
import Data.List((\\))
import Data.Maybe

import System.IO

import Control.Monad.State
import Math.SMT.Yices.Pipe
import qualified Math.SMT.Yices.Syntax as Y

import Seri.Lambda
import Seri.Target.Monomorphic.Monomorphic
import Seri.Target.Elaborate
import Seri.Target.Yices.Compiler
import Seri.Target.Yices.Yices


data YicesState = YicesState {
    ys_decls :: [Dec],
    ys_ipc :: YicesIPC,
    ys_dh :: Handle,
    ys_freeid :: Integer
}

type YicesMonad = StateT YicesState IO

sendCmds :: [Y.CmdY] -> YicesIPC -> Handle -> IO ()
sendCmds cmds ipc dh = do
    hPutStr dh (unlines (map show cmds))
    runCmdsY' ipc cmds

runCmds :: [Y.CmdY] -> YicesMonad ()
runCmds cmds = do
    ipc <- gets ys_ipc
    dh <- gets ys_dh
    lift $ sendCmds cmds ipc dh

-- Tell yices about any types or expressions needed to refer to the given
-- expression, and return the monomorphized expression.
declareNeeded :: Monomorphic a => Env -> a -> YicesMonad a
declareNeeded env x = do
  decs <- gets ys_decls
  let (mds, me) = monomorphic env x
  let (pdecls, _) = sort mds
  let newdecls = pdecls \\ decs
  modify $ \ys -> ys { ys_decls = decs ++ newdecls }
  runCmds (yDecs newdecls)
  return me

runQuery :: Rule YicesMonad -> Env -> Exp -> YicesMonad Exp
runQuery gr env e = do
    elaborated <- elaborate gr env e
    case elaborated of
        (AppE (VarE (Sig "query" _)) arg) -> do
            dh <- gets ys_dh
            ipc <- gets ys_ipc
            res <- lift $ checkY ipc
            lift $ hPutStrLn dh $ ">> check returned: " ++ show res 
            case res of 
                Unknown _ -> return $ ConE (Sig "Unknown" (AppT (ConT "Answer") (typeof arg)))
                Sat evidence -> return $ AppE (ConE (Sig "Satisfiable" (AppT (ConT "Answer") (typeof arg)))) (realize (yassignments evidence) arg)
                _ -> return $ ConE (Sig "Unsatisfiable" (AppT (ConT "Answer") (typeof arg)))
        (VarE (Sig "free" (AppT (ConT "Query") t))) -> do
            fid <- gets ys_freeid
            modify $ \ys -> ys {ys_freeid = fid+1}
            
            t' <- declareNeeded env t
            runCmds [Y.DEFINE ("free_" ++ show fid, yType t') Nothing]
            return (AppE (VarE (Sig "~free" (arrowsT [integerT, t]))) (IntegerE fid))
        (AppE (VarE (Sig "assert" _)) p) -> do
            p' <- declareNeeded env p
            runCmds [Y.ASSERT (yExp p')]
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
yType t = fromYCM $ compile_type smtY smtY t

yDecs :: [Dec] -> [Y.CmdY]
yDecs = compile_decs smtY

yExp :: Exp -> Y.ExpY
yExp e = fromYCM $ compile_exp smtY smtY e

data RunOptions = RunOptions {
    debugout :: Maybe FilePath,
    yicesexe :: FilePath
} deriving(Show)
            
runYices :: [Y.CmdY] -> Rule YicesMonad -> RunOptions -> Env -> Exp -> IO Exp
runYices primlib gr opts env e = do
    dh <- openFile (fromMaybe "/dev/null" (debugout opts)) WriteMode

    -- We set NoBuffering, because otherwise the output gets chopped off for
    -- longer outputs.
    hSetBuffering dh NoBuffering
    ipc <- createYicesPipe (yicesexe opts) ["-tc"]
    case ipc of
        (Just hin, Just hout, _, _) -> hSetBuffering hin NoBuffering
    sendCmds (includes smtY ++ primlib) ipc dh
    (x, _) <- runStateT (runQuery gr env e) (YicesState [] ipc dh 1)
    hClose dh
    return x
    

smtY :: Compiler
smtY =
  let ye :: Compiler -> Exp -> YCM Y.ExpY
      ye _ (AppE (VarE (Sig "~free" _)) (IntegerE id)) = return $ Y.VarE ("free_" ++ show id)
      ye _ e = fail $ "smtY does not apply: " ++ pretty e

      yt :: Compiler -> Type -> YCM Y.TypY
      yt _ t = fail $ "smtY does not apply: " ++ pretty t
  in compilers [Compiler [] ye yt, yicesY]


-- Given the evidence returned by a yices query, extract the free variable
-- assignments.
yassignments :: [Y.ExpY] -> [(Integer, Exp)]
yassignments = concat . map assignment

assignment :: Y.ExpY -> [(Integer, Exp)]
assignment (Y.VarE ('f':'r':'e':'e':'_':id) Y.:= e)
    = case (antiyices e) of
        Just e -> [(read id, e)]
        Nothing -> []
assignment (e Y.:= Y.VarE ('f':'r':'e':'e':'_':id))
    = case (antiyices e) of
        Just e -> [(read id, e)]
        Nothing -> []

antiyices :: (Monad m) => Y.ExpY -> m Exp
antiyices (Y.LitB True) = return $ ConE (Sig "True" (ConT "Bool"))
antiyices (Y.LitB False) = return $ ConE (Sig "False" (ConT "Bool"))
antiyices (Y.LitI i) = return $ IntegerE i
antiyices (Y.VarE n) | isUpper (head n) = return $ ConE (Sig n UnknownT)
antiyices (Y.APP f [x]) = do
    f' <- antiyices f
    x' <- antiyices x
    return $ AppE f' x'
antiyices x = fail $ "TODO: antiyices: " ++ show x

-- Apply free variable assignements to the given expression.
realize :: [(Integer, Exp)] -> Exp -> Exp
realize as =
    let qexp :: Exp -> Exp
        qexp (AppE (VarE (Sig "~free" (AppT (AppT (ConT "->") (ConT "Integer")) t))) (IntegerE fid))
            = case lookup fid as of
                Just e -> e
                Nothing -> (VarE (Sig "undefined" t))
        qexp e = e
    in everywhere (mkT qexp)

