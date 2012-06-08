
module Seri.SMT.Yices (runYices, yicesR) where

import Data.List((\\))

import Control.Monad.State
import Math.SMT.Yices.Pipe
import qualified Math.SMT.Yices.Syntax as Y

import Seri.Target.Yices.Compiler
import Seri.Target.Yices.Yices

import Seri.Lambda
import Seri.Utils.Ppr

data YicesState = YicesState {
    ys_decls :: [Dec],
    ys_ipc :: YicesIPC,
    ys_freeid :: Integer
}

type YicesMonad = StateT YicesState IO

yicesR :: Rule YicesMonad
yicesR = rules [bindR, nobindR]

bindR :: Rule YicesMonad
bindR = Rule $ \gr e ->
    case val e of
        (AppE (AppE (PrimE (Sig "bind_query" _)) x) f) -> do
          result <- runQuery gr (withenv e x)
          return $ Just (AppE f result)
        _ -> return Nothing

nobindR :: Rule YicesMonad
nobindR = Rule $ \gr e ->
    case val e of
        (AppE (AppE (PrimE (Sig "nobind_query" _)) x) y) -> do
          runQuery gr (withenv e x)
          return $ Just y
        _ -> return Nothing

runQuery :: Rule YicesMonad -> Env Exp -> YicesMonad Exp
runQuery gr e = do
    elaborated <- elaborate gr e
    case elaborated of
        (AppE (PrimE (Sig "query" _)) arg) -> do
            ipc <- gets ys_ipc
            res <- lift $ checkY ipc
            -- TODO: Read the evidence and return the appropriate expression
            -- when satisfiable.
            case res of 
                Unknown _ -> return $ ConE (Sig "Unknown" (AppT (ConT "Answer") (typeof arg)))
                Sat _ -> return $ AppE (ConE (Sig "Satisfiable" (AppT (ConT "Answer") (typeof arg)))) (PrimE (Sig "undefined" (typeof arg)))
                _ -> return $ ConE (Sig "Unsatisfiable" (AppT (ConT "Answer") (typeof arg)))
        (PrimE (Sig "free" (AppT (ConT "Query") t))) -> do
            ipc <- gets ys_ipc
            fid <- gets ys_freeid
            modify $ \ys -> ys {ys_freeid = fid+1}
            lift $ runCmdsY' ipc [Y.DEFINE ("free_" ++ show fid, yType t) Nothing]
            return (AppE (PrimE (Sig "realize" (AppT (AppT (ConT "->") (AppT (ConT "Free") t)) t))) (AppE (ConE (Sig "Free" (AppT (AppT (ConT "->") (ConT "Integer")) (AppT (ConT "Free") t)))) (IntegerE fid)))
        (AppE (PrimE (Sig "assert" _)) p) -> do
            ipc <- gets ys_ipc

            -- Tell yices about any new functions or types needed to
            -- assert the predicate.
            decs <- gets ys_decls
            let (pdecls, []) = sort $ decls (minimize (withenv e p))
            let newdecls = pdecls \\ decs
            modify $ \ys -> ys { ys_decls = decs ++ newdecls }
            lift $ runCmdsY' ipc (yDecs newdecls)

            -- Assert the predicate.
            let (cmds, py) = yExp p
            lift $ runCmdsY' ipc (cmds ++ [Y.ASSERT py])
            return (ConE (Sig "()" (ConT "()")))
        x -> error $ "unknown Query: " ++ render (ppr x)


yicespath = "/home/ruhler/sri/scratch/yices/yices-1.0.34/bin/tyices"

yType :: Type -> Y.TypY
yType t = case compile_type smtY smtY t of
              Just yt -> yt
              Nothing -> error $ "failed: yType " ++ render (ppr t)

yDecs :: [Dec] -> [Y.CmdY]
yDecs = compile_decs smtY

yExp :: Exp -> ([Y.CmdY], Y.ExpY)
yExp e = case compile_exp smtY smtY e of
              Just ye -> ye
              Nothing -> error $ "failed: yExp " ++ render (ppr e)
            
runYices :: Rule YicesMonad -> Env Exp -> IO Exp
runYices gr e = do
    ipc <- createYicesPipe yicespath ["-tc"]
    runCmdsY' ipc (includes smtY)
    (x, _) <- runStateT (runQuery gr e) (YicesState [] ipc 1)
    return x
    

smtY :: Compiler
smtY =
  let ye :: Compiler -> Exp -> Maybe ([Y.CmdY], Y.ExpY)
      ye _ (AppE (PrimE (Sig "realize" _)) (AppE (ConE (Sig "Free" _)) (IntegerE id))) = Just $ ([], Y.VarE ("free_" ++ show id))
      ye _ _ = Nothing

      yt :: Compiler -> Type -> Maybe Y.TypY
      yt _ _ = Nothing
  in compilers [Compiler [] ye yt, yicesY]
