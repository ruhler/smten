

module SeriC (c)
  where

import Control.Monad.State

import qualified Seri as S
import C

data CState = CState {
    name :: Integer,
    decs :: [Dec]
}

c :: S.Exp -> ([Dec], Exp)
c se
 =  let (e, cs) = runState (c' se) $ CState 0 []
    in (decs cs, e)
    

c' :: S.Exp -> State CState Exp
c' (S.IntegerE i) = return $ IntE i
c' (S.AddE a b) = do
    av <- c' a
    bv <- c' b
    return $ AddE av bv
c' (S.MulE a b) = do
    av <- c' a
    bv <- c' b
    return $ MulE av bv
c' (S.AppE _ a b) = do
    f <- c' a
    x <- c' b
    return $ AppE f [x]
c' (S.LamE (S.ArrowT tn te) argname e) = do
    CState n ds <- get
    let lname = "l" ++ show n
    eb <- c' e
    let nd = FunD lname [(ctype tn, argname)] (ctype te) [ReturnS eb]
    put $ CState (n+1) (nd:ds)
    return $ VarE lname
c' (S.VarE _ n) = return $ VarE n

ctype :: S.Type -> Type
ctype S.IntegerT = IntT
ctype x = error $ "TODO: ctype " ++ show x

