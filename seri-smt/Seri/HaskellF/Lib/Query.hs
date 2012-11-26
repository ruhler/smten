
module Seri.HaskellF.Lib.Query (
    Query, RunOptions(..), Answer(..),
    query, queryS, assert, runQuery, freebool, freeinteger,
    ) where

import Control.Monad.State
import Data.Functor((<$>))
import System.IO

import qualified Seri.HaskellF.Lib.Prelude as F
import qualified Seri.SMT.Solver as SMT
import qualified Seri.SMT.Syntax as SMT
import Seri.SMT.Translate
import Seri.Sig
import Seri.Name
import Seri.Failable
import Seri.Type
import Seri.Exp


data Answer a = Satisfiable a | Unsatisfiable | Unknown
    deriving (Eq, Show)

data QS = QS {
    qs_ctx :: SMT.Solver,
    qs_dh :: Maybe Handle,
    qs_freeid :: Integer,
    qs_qs :: Compilation,
    qs_freevars :: [Sig]
}

type Query = StateT QS IO

sendCmds :: [SMT.Command] -> SMT.Solver -> Maybe Handle -> IO ()
sendCmds cmds ctx Nothing = mapM_ (SMT.run ctx) cmds
sendCmds cmds ctx (Just dh) = do
    hPutStr dh (unlines (map (SMT.pretty ctx) cmds))
    mapM_ (SMT.run ctx) cmds

runCmds :: [SMT.Command] -> Query ()
runCmds cmds = do
    ctx <- gets qs_ctx
    dh <- gets qs_dh
    lift $ sendCmds cmds ctx dh

check :: Query SMT.Result
check = do
    ctx <- gets qs_ctx
    debug (SMT.pretty ctx SMT.Check)
    res <- lift $ SMT.check ctx
    debug $ "; check returned: " ++ show res
    return res

-- Output a line to the debug output.
debug :: String -> Query ()
debug msg = do
    dh <- gets qs_dh
    case dh of
        Nothing -> return ()
        Just h -> lift $ hPutStrLn h msg

freevar :: Query Name
freevar = do
    fid <- gets qs_freeid
    modify $ \qs -> qs { qs_freeid = fid+1 }
    return $ freename fid

freename :: Integer -> Name
freename id = name $ "free~" ++ show id

data RunOptions = RunOptions {
    -- | Optionally output debug info to the given file.
    ro_debugout :: Maybe FilePath,
    ro_solver :: SMT.Solver
}
            
mkQS :: RunOptions -> IO QS
mkQS opts = do
    dh <- case ro_debugout opts of
            Nothing -> return Nothing
            Just dbgfile -> do
                h <- openFile dbgfile WriteMode
                hSetBuffering h NoBuffering
                return (Just h)

    return $ QS {
        qs_ctx = ro_solver opts,
        qs_dh = dh,
        qs_freeid = 1,
        qs_qs = compilation,
        qs_freevars = []
    }

-- Note: it's possible to leak free variables with this function.
-- You should not return anything from the first query which could contain a
-- free variable, otherwise who knows what will happen.
runQuery :: RunOptions -> Query a -> IO a
runQuery opts q = do
    qs <- mkQS opts
    evalStateT q qs


realizefree :: Sig -> Query Exp
realizefree (Sig nm t) | t == boolT = do
    ctx <- gets qs_ctx
    bval <- lift $ SMT.getBoolValue ctx (unname nm)
    debug $ "; " ++ unname nm ++ " is " ++ show bval
    return $ boolE bval
realizefree (Sig nm t) | t == integerT = do
    ctx <- gets qs_ctx
    ival <- lift $ SMT.getIntegerValue ctx (unname nm)
    debug $ "; " ++ unname nm ++ " is " ++ show ival
    return $ integerE ival

-- | Check if the current assertions are satisfiable.
query :: (F.Symbolic__ a) => a -> Query (Answer a)
query r = do
  res <- check
  case res of 
      SMT.Satisfiable -> do
        freevars <- gets qs_freevars
        freevals <- mapM realizefree freevars
        let flookup :: Name -> Maybe Exp
            flookup s = lookup s (zip [n | Sig n _ <- freevars] freevals)
        return $ Satisfiable (F.__substitute flookup r)
      SMT.Unsatisfiable -> return Unsatisfiable
      _ -> return Unknown

-- | Allocate a free expression of boolean type.
freebool :: Query Exp
freebool = do
  free <- freevar
  let s = Sig free boolT
  modify $ \qs -> qs { qs_freevars = s : qs_freevars qs }
  runCmds [SMT.Declare (unname free) SMT.BoolT]
  return $ VarE s

freeinteger :: Query Exp
freeinteger = do
  free <- freevar
  let s = Sig free integerT
  modify $ \qs -> qs { qs_freevars = s : qs_freevars qs }
  runCmds [SMT.Declare (unname free) SMT.IntegerT]
  return $ VarE s

smte :: Exp -> Query SMT.Expression
smte e = do
    qs <- gets qs_qs 
    let mkye :: CompilationM ([SMT.Command], SMT.Expression)
        mkye = do
          ye <- smtE e
          cmds <- smtD
          return (cmds, ye)
    ((cmds, ye), qs') <- lift . attemptIO $ runCompilation mkye qs
    modify $ \s -> s { qs_qs = qs' }
    runCmds cmds
    return ye

assert :: Exp -> Query ()
assert e = do
    e' <- smte e
    runCmds [SMT.Assert e']

-- | Run the given query in its own scope and return the result.
-- Note: it's possible to leak free variables with this function.
-- You should not return anything from the first query which could contain a
-- free variable, otherwise who knows what will happen.
queryS :: Query a -> Query a
queryS q = do
  runCmds [SMT.Push]
  v <- q
  runCmds [SMT.Pop]
  return v

