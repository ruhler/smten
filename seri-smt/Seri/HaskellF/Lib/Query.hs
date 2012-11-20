
module Seri.HaskellF.Lib.Query (
    Query, RunOptions(..), Answer(..),
    query, queryS, assert, runQuery, freebool,
    ) where

import Control.Monad.State
import Data.Functor((<$>))
import System.IO

import qualified Seri.HaskellF.Lib.Prelude as F
import qualified Seri.SMT.Solver as SMT
import qualified Seri.SMT.Syntax as SMT


type Name = String

data Answer a = Satisfiable a | Unsatisfiable | Unknown
    deriving (Eq, Show)

data QS = QS {
    qs_ctx :: SMT.Solver,
    qs_dh :: Maybe Handle,
    qs_freeid :: Integer,
    qs_freevars :: [Name]
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
freename id = "free~" ++ show id

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
        qs_freevars = []
    }

-- Note: it's possible to leak free variables with this function.
-- You should not return anything from the first query which could contain a
-- free variable, otherwise who knows what will happen.
runQuery :: RunOptions -> Query a -> IO a
runQuery opts q = do
    qs <- mkQS opts
    evalStateT q qs


realizefree :: Name -> Query Bool
realizefree nm = do
    ctx <- gets qs_ctx
    bval <- lift $ SMT.getBoolValue ctx nm
    debug $ "; " ++ nm ++ " is " ++ show bval
    return bval

-- | Check if the current assertions are satisfiable.
query :: (F.Symbolic__ a) => a -> Query (Answer a)
query r = do
  res <- check
  case res of 
      SMT.Satisfiable -> do
        freevars <- gets qs_freevars
        freevals <- mapM realizefree freevars
        let flookup :: SMT.Symbol -> Maybe SMT.Expression
            flookup s = SMT.boolE <$> lookup s (zip freevars freevals)
        return $ Satisfiable (F.__substitute flookup r)
      SMT.Unsatisfiable -> return Unsatisfiable
      _ -> return Unknown

-- | Allocate a free expression of boolean type.
freebool :: Query F.Bool
freebool = do
  free <- freevar
  modify $ \qs -> qs { qs_freevars = free : qs_freevars qs }
  runCmds [SMT.Declare free SMT.BoolT]
  return $ F.__free free

assert :: F.Bool -> Query ()
assert b = runCmds [SMT.Assert (F.__toSMT b)]

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

