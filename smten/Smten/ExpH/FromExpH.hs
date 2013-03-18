
{-# LANGUAGE PatternGuards #-}

module Smten.ExpH.FromExpH (
    fromExpH
  ) where

import Control.Monad.State
import Data.Functor ((<$>))
import qualified Data.HashMap as Map
import qualified Data.Set as Set

import Smten.Sig
import Smten.Name
import Smten.Type
import Smten.Exp
import Smten.ExpH.ExpH
import Smten.ExpH.Sugar
import Smten.ExpH.SmtenEHs
import Smten.Strict

-- Translate back to the normal Exp representation
fromExpH :: ExpH -> Exp
fromExpH e = {-# SCC "CONVERT" #-} convert ({-# SCC "SHARING" #-} sharing e) e

data Use = Multi | Single
    deriving (Eq)

-- Find all the subexpressions in the given expression which should be shared.
sharing :: ExpH -> Set.Set EID
sharing e =
  let traverse :: ExpH -> State (Map.Map EID Use) (Set.Set EID)
      traverse e
        | Just id <- getid e = do
            m <- get
            case Map.lookup id m of
               Nothing -> do
                    put (Map.insert id Single m)
                    subtraverse e
               Just Single -> do    
                    put (Map.insert id Multi m)
                    return $! Set.singleton id
               Just Multi -> return Set.empty
        | otherwise = return Set.empty

      subtraverse :: ExpH -> State (Map.Map EID Use) (Set.Set EID)
      subtraverse e
        | ConEH _ _ _ xs <- e = Set.unions <$!> mapM traverse xs
        | PrimEH _ _ _ _ xs <- e = Set.unions <$!> mapM traverse xs
        | IfEH _ _ x y n <- e = Set.unions <$!> mapM traverse [x, y, n]
        | otherwise = return $ Set.empty
  in evalState (traverse e) Map.empty

data Defined = Defined {
    df_defs :: [(EID, Exp)],
    df_done :: Set.Set EID
}

convert :: Set.Set EID -> ExpH -> Exp
convert share e =
  let -- Generate the definition for this expression.
      defineM :: ExpH -> State Defined Exp
      defineM e
        | LitEH l <- e = return $ LitE l
        | ConEH _ n t xs <- e = do
            xs' <- mapM useM xs
            let t' = arrowsT $ (map typeof xs') ++ [t]
            return $ appsE (ConE (Sig n t')) xs'
        | VarEH s <- e = return $ VarE s
        | PrimEH _ n t _ xs <- e = do
            xs' <- mapM useM xs
            let t' = arrowsT $ (map typeof xs') ++ [t]
            return $ appsE (varE (Sig n t')) xs'
        | LamEH _ (Sig nm t) _ f <- e = do
            let s' = identify $ \x -> Sig (nm `nappend` (name (show x))) t
            b <- useM (f (VarEH s'))
            return $ LamE s' b
        | IfEH _ _ arg yes no <- e = do
            arg' <- useM arg
            yes' <- useM yes
            no' <- useM no
            return $ ifE arg' yes' no'
        | ErrorEH t s <- e = return $
            appE (varE (Sig (name "Prelude.error") (arrowT stringT t))) (stringE s)
            

      -- Generate the use for this expression.
      -- So, if it's shared, turns into a VarE.
      useM :: ExpH -> State Defined Exp
      useM e
        | Just id <- getid e
        , Set.member id share = do
            done <- gets df_done
            let var = VarE (Sig (nameof id) (typeof e))
            case Set.member id done of
                True -> return var
                False -> do
                   v <- defineM e
                   modifyS $ \df -> df {
                      df_defs = (id, v) : df_defs df,
                      df_done = Set.insert id (df_done df) }
                   return var
        | otherwise = defineM e
    
      (body, defined) = runState (defineM e) (Defined [] Set.empty)
      bindings = reverse (df_defs defined)

      nameof :: EID -> Name
      nameof x = name $ "s~" ++ show x
  in letsE [(Sig (nameof x) (typeof v), v) | (x, v) <- bindings] body

