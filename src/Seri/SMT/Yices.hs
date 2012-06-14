
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
-- expression.
declareNeeded :: Env Exp -> YicesMonad ()
declareNeeded x = do
  decs <- gets ys_decls
  let (pdecls, _) = sort $ decls (monomorphic x)
  let newdecls = pdecls \\ decs
  modify $ \ys -> ys { ys_decls = decs ++ newdecls }
  runCmds (yDecs newdecls)

runQuery :: Rule YicesMonad -> Env Exp -> YicesMonad Exp
runQuery gr e = do
    elaborated <- elaborate gr e
    case elaborated of
        (AppE (PrimE (Sig "query" _)) arg) -> do
            dh <- gets ys_dh
            ipc <- gets ys_ipc
            res <- lift $ checkY ipc
            lift $ hPutStrLn dh $ ">> check returned: " ++ show res 
            case res of 
                Unknown _ -> return $ ConE (Sig "Unknown" (AppT (ConT "Answer") (typeof arg)))
                Sat evidence -> return $ AppE (ConE (Sig "Satisfiable" (AppT (ConT "Answer") (typeof arg)))) (realize (yassignments evidence) arg)
                _ -> return $ ConE (Sig "Unsatisfiable" (AppT (ConT "Answer") (typeof arg)))
        (PrimE (Sig "free" (AppT (ConT "Query") t))) -> do
            fid <- gets ys_freeid
            modify $ \ys -> ys {ys_freeid = fid+1}
            
            declareNeeded (withenv e (PrimE (Sig "foo" t)))
            runCmds [Y.DEFINE ("free_" ++ show fid, yType t) Nothing]
            return (AppE (PrimE (Sig "realize" (AppT (AppT (ConT "->") (AppT (ConT "Free") t)) t))) (AppE (ConE (Sig "Free" (AppT (AppT (ConT "->") (ConT "Integer")) (AppT (ConT "Free") t)))) (IntegerE fid)))
        (AppE (PrimE (Sig "assert" _)) p) -> do
            declareNeeded (withenv e p)
            runCmds [Y.ASSERT (yExp p)]
            return (ConE (Sig "()" (ConT "()")))
        (AppE (PrimE (Sig "queryS" _)) q) -> do
            odecls <- gets ys_decls
            runCmds [Y.PUSH]
            x <- runQuery gr (withenv e q)
            let q' = AppE (PrimE (Sig "query" undefined)) x
            y <- runQuery gr (withenv e q')
            runCmds [Y.POP]
            modify $ \ys -> ys { ys_decls = odecls }
            return y
        (AppE (PrimE (Sig "return_query" _)) x) -> return x
        (AppE (AppE (PrimE (Sig "bind_query" _)) x) f) -> do
          result <- runQuery gr (withenv e x)
          runQuery gr (withenv e (AppE f result))
        (AppE (AppE (PrimE (Sig "nobind_query" _)) x) y) -> do
          runQuery gr (withenv e x)
          runQuery gr (withenv e y)
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
            
runYices :: [Y.CmdY] -> Rule YicesMonad -> RunOptions -> Env Exp -> IO Exp
runYices primlib gr opts e = do
    dh <- openFile (fromMaybe "/dev/null" (debugout opts)) WriteMode
    ipc <- createYicesPipe (yicesexe opts) ["-tc"]
    sendCmds (includes smtY ++ primlib) ipc dh
    (x, _) <- runStateT (runQuery gr e) (YicesState [] ipc dh 1)
    hClose dh
    return x
    

smtY :: Compiler
smtY =
  let ye :: Compiler -> Exp -> YCM Y.ExpY
      ye _ (AppE (PrimE (Sig "realize" _)) (AppE (ConE (Sig "Free" _)) (IntegerE id))) = return $ Y.VarE ("free_" ++ show id)
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
        qexp (AppE (PrimE (Sig "realize" (AppT (AppT (ConT "->") (AppT (ConT "Free") t)) _))) (AppE (ConE (Sig "Free" (AppT (AppT (ConT "->") (ConT "Integer")) (AppT (ConT "Free") _)))) (IntegerE fid)))
            = case lookup fid as of
                Just e -> e
                Nothing -> (PrimE (Sig "undefined" t))
        qexp e = e
    in everywhere (mkT qexp)

