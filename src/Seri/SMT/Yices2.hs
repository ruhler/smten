
module Seri.SMT.Yices2 (RunOptions(..), runYices) where

import Data.Generics
import Data.Char(isUpper)
import Data.List((\\))
import Data.Maybe

import System.IO

import Control.Monad.State
import qualified Yices2.Syntax as Y
import qualified Yices2.Yices2 as Y

import Seri.Failable
import Seri.Lambda
import Seri.Target.Monomorphic.Monomorphic
import Seri.Target.Elaborate
import Seri.Target.Inline
import Seri.Target.Yices.Yices2


data YicesState = YicesState {
    ys_decls :: [Dec],
    ys_ctx :: Y.Context,
    ys_dh :: Handle,
    ys_freeid :: Integer,
    ys_ys :: YS,
    ys_idepth :: Integer
}

type YicesMonad = StateT YicesState IO

sendCmds :: [Y.Command] -> Y.Context -> Handle -> IO ()
sendCmds cmds ctx dh = do
    hPutStr dh (unlines (map Y.pretty cmds))
    mapM_ (Y.run ctx) cmds

runCmds :: [Y.Command] -> YicesMonad ()
runCmds cmds = do
    ctx <- gets ys_ctx
    dh <- gets ys_dh
    lift $ sendCmds cmds ctx dh

check :: YicesMonad Y.SMTStatus
check = do
    debug "(check)"
    ctx <- gets ys_ctx
    res <- lift $ Y.check ctx
    debug $ "; check returned: " ++ show res
    return res

-- Output a line to the debug output.
debug :: String -> YicesMonad ()
debug msg = do
    dh <- gets ys_dh
    lift $ hPutStrLn dh msg

freevar = do
    fid <- gets ys_freeid
    modify $ \ys -> ys { ys_freeid = fid+1 }
    return $ freename fid

-- Tell yices about any types or expressions needed to refer to the given
-- expression, and return the monomorphized expression.
declareNeeded :: (Monomorphic a, Ppr a) => Env -> a -> YicesMonad a
declareNeeded env x = do
  decs <- gets ys_decls
  let (mds, me) = monomorphic env x
  let (pdecls, _) = sort mds
  let newdecls = pdecls \\ decs
  modify $ \ys -> ys { ys_decls = decs ++ newdecls }
  cmds <- yDecs newdecls
  runCmds cmds
  return me

freename :: Integer -> String
freename id = "free~" ++ show id

isfreename :: String -> Bool
isfreename nm = "free~" == take 5 nm

runQuery :: Rule YicesMonad -> Env -> Exp -> YicesMonad Exp
runQuery gr env e = do
    elaborated <- elaborate gr env e
    case elaborated of
        (AppE (VarE (Sig "query" _)) arg) -> do
            res <- check
            case res of 
                Y.STATUS_SAT -> do
                    arg' <- realize arg
                    return $ AppE (ConE (Sig "Satisfiable" (AppT (ConT "Answer") (typeof arg)))) arg'
                Y.STATUS_UNSAT -> return $ ConE (Sig "Unsatisfiable" (AppT (ConT "Answer") (typeof arg)))
                _ -> return $ ConE (Sig "Unknown" (AppT (ConT "Answer") (typeof arg)))
        (VarE (Sig "free" (AppT (ConT "Query") t))) -> do
            t' <- declareNeeded env t
            free <- freevar
            runCmds [Y.Define (yicesN free) (yType t') Nothing]
            return (VarE (Sig free t))
        (AppE (VarE (Sig "assert" _)) p) -> do
            --lift $ putStrLn $ "poly: " ++ pretty p
            idepth <- gets ys_idepth
            let inlined = inline idepth env p
            --lift $ putStrLn $ "inli: " ++ pretty inlined
            simplified <- elaborate simplifyR env inlined
            --lift $ putStrLn $ "simp: " ++ pretty simplified
            p' <- declareNeeded env simplified
            yp <- yExp p'
            true <- yExp trueE
            runCmds [Y.Assert (Y.eqE true yp)]
            return (ConE (Sig "()" (ConT "()")))
        (AppE (VarE (Sig "queryS" _)) q) -> do
            odecls <- gets ys_decls
            runCmds [Y.Push]
            x <- runQuery gr env q
            let q' = AppE (VarE (Sig "query" undefined)) x
            y <- runQuery gr env q'
            runCmds [Y.Pop]
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


yType :: Type -> Y.Type
yType t = surely $ yicesT t

yDecs :: [Dec] -> YicesMonad [Y.Command]
yDecs ds = do
    ys <- gets ys_ys
    (cmds, ys') <- lift . attemptIO $ runYCompiler (mapM yicesD ds) ys
    modify $ \s -> s { ys_ys = ys' }
    return $ concat cmds

yExp :: Exp -> YicesMonad Y.Expression
yExp e = do
    ys <- gets ys_ys
    ((cmds, e'), ys') <- lift . attemptIO $ runYCompiler (yicesE e) ys
    modify $ \s -> s { ys_ys = ys' }
    runCmds cmds
    return e'

data RunOptions = RunOptions {
    debugout :: Maybe FilePath,
    inlinedepth :: Integer
} deriving(Show)
            
runYices :: Rule YicesMonad -> RunOptions -> Env -> Exp -> IO Exp
runYices gr opts env e = do
    dh <- openFile (fromMaybe "/dev/null" (debugout opts)) WriteMode

    -- We set NoBuffering, because otherwise the output gets chopped off for
    -- longer outputs.
    hSetBuffering dh NoBuffering
    Y.init
    ctx <- Y.mkctx
    let mono = fst $ monomorphic env e
    (x, _) <- runStateT (runQuery gr env e) (YicesState [] ctx dh 1 (ys mono) (inlinedepth opts))
    hClose dh
    Y.exit
    return x
    


-- | Given a free variable name and corresponding seri type, return the value
-- of that free variable from the yices model.
realizefree :: String -> Type -> YicesMonad Exp
realizefree nm t | t == integerT = do
    debug $ "; realize integer: " ++ nm
    res <- check
    case res of
        Y.STATUS_SAT -> return ()
        _ -> error $ "realize free expected Satisfiable, but wasn't"
    ctx <- gets ys_ctx
    ival <- lift $ Y.getIntegerValue ctx (yicesN nm)
    debug $ "; " ++ nm ++ " is " ++ show ival
    return (integerE ival)
realizefree nm t@(AppT (AppT (ConT "->") _) _)
  = error $ "TODO: realizefree type " ++ pretty t
realizefree nm t =
  let ConT dt = head $ unappsT t
      trycons :: [Con] -> YicesMonad Exp
      trycons [] = return $ VarE (Sig "undefined" t)
      trycons (Con cn ts : cs) = do
            runCmds [Y.Push]
            free <- sequence $ replicate (length ts) freevar
            runCmds [Y.Define (yicesN f) (yType t) Nothing | (f, t) <- zip free ts]
            let args = [VarE (Sig n t) | (n, t) <- zip free ts]
            want <- yExp (appsE $ (ConE (Sig cn (ConT dt))) : args)
            runCmds [Y.Assert (Y.eqE (Y.varE (yicesN nm)) want)]
            res <- check
            case res of
                Y.STATUS_SAT -> do
                    argvals <- sequence [realizefree n t | (n, t) <- zip free ts]
                    runCmds [Y.Pop]
                    return (appsE $ (ConE (Sig cn (ConT dt))) : argvals)
                _ -> do
                    runCmds [Y.Pop]
                    trycons cs
            
  in do
    debug $ "; realize: " ++ nm ++ " :: " ++ pretty t
    env <- gets ys_decls
    DataD _ _ cs <- lift . attemptIO $ lookupDataD env dt
    trycons cs
    

-- | Update the free variables in the given expression based on the current
-- yices model.
realize :: Exp -> YicesMonad Exp
realize = 
  let f :: Exp -> YicesMonad Exp
      f (VarE (Sig nm ty)) | isfreename nm = realizefree nm ty
      f e = return e
  in everywhereM (mkM f)

