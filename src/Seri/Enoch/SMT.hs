
module Seri.Enoch.SMT (
    Query, Answer(..), 
    free, assert, query, run, run',
    runQuery, Y.RunOptions(..),
 ) where

import Control.Monad.State.Strict

import Seri.Lambda hiding (free, query)
import Seri.Enoch.Enoch
import Seri.Enoch.Prelude

import qualified Seri.SMT.Yices2 as Y

data Answer a =
    Satisfiable a
  | Unsatisfiable
  | Unknown
    deriving (Eq, Show)

instance (Seriable a) => Seriable (Answer a) where
    pack (Satisfiable x) =
      let ct = arrowsT [serit x, ConT (name "Answer")]

          con :: TExp (a -> Answer a)
          con = TExp $ ConE (Sig (name "Satisfiable") ct)
      in apply con (pack x)

    unpack (TExp (AppE (ConE (Sig n _)) x)) | n == name "Satisfiable" = do
        a <- unpack (TExp x)
        return $ Satisfiable a
    unpack (TExp (ConE (Sig n _))) | n == name "Unsatisfiable" = return Unsatisfiable
    unpack (TExp (ConE (Sig n _))) | n == name "Unknown" = return Unknown
    unpack _ = Nothing

    serit x =
      let t :: Answer a -> a
          t _ = undefined
      in appsT [ConT (name "Answer"), serit (t x)]
                

type Query = StateT Y.SMTQuerier IO

queryT :: Type -> Type
queryT t = appsT [ConT (name "Query"), t]

free :: (Seriable a) => Query (TExp a)
free = 
  let t1 :: (Seriable a) => a -> (Type -> b) -> b
      t1 x f = f (serit x)

      t2 :: Query (TExp a) -> a
      t2 _ = undefined

      freeE :: Type -> Query (TExp a)
      freeE t = run (TExp $ VarE (Sig (name "Seri.SMT.SMT.free") (queryT t)))

      me = t1 (t2 me) freeE
  in me

assert :: TExp Bool -> Query ()
assert p =
  let ty = arrowsT [boolT, queryT unitT]
      
      assertE :: TExp (Bool -> Query ())
      assertE = TExp $ VarE (Sig (name "Seri.SMT.SMT.assert") ty)
  in run' (apply assertE p)

query :: (Seriable a) => TExp a -> Query (Answer a)
query x = 
  let t1 :: TExp a -> a
      t1 _ = undefined

      t2 :: TExp a -> Answer a
      t2 _ = undefined

      ty = arrowsT [serit (t1 x), queryT (serit (t2 x))]

      queryE :: TExp (a -> Query (Answer a))
      queryE = TExp $ (VarE (Sig (name "Seri.SMT.SMT.query") ty))
  in run' (apply queryE x)

run :: TExp (Query a) -> Query (TExp a)
run (TExp x) = do
     s <- get
     (r, s') <- liftIO $ Y.runQuery s x
     put s'
     return (TExp r)

run' :: (Seriable a) => TExp (Query a) -> Query a
run' x = fmap unpack' $ run x

runQuery :: Y.RunOptions -> Env -> Query a -> IO a
runQuery opts env q = do
    querier <- Y.mkQuerier opts env
    evalStateT q querier
    
    

