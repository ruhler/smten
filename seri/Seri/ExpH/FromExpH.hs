
{-# LANGUAGE PatternGuards #-}

module Seri.ExpH.FromExpH (
    fromExpH
  ) where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Seri.Sig
import Seri.Name
import Seri.Type
import Seri.Exp
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar
import Seri.ExpH.SeriEHs

-- Translate back to the normal Exp representation
fromExpH :: ExpH -> Exp
fromExpH e = convert (sharing e) e

data Use = Multi | Single
    deriving (Eq)

-- Find all the subexpressions in the given expression which should be shared.
sharing :: ExpH -> Set.Set ID
sharing e =
  let traverse :: ExpH -> State (Map.Map ID Use) ()
      traverse e
        | Just id <- getid e = do
            m <- get
            case Map.lookup id m of
               Just Single -> put (Map.insert id Multi m)
               Just Multi -> return ()
               Nothing -> put (Map.insert id Single m) >> subtraverse e
        | otherwise = return ()

      subtraverse :: ExpH -> State (Map.Map ID Use) ()
      subtraverse e
        | ConEH _ _ _ xs <- e = mapM_ traverse xs
        | PrimEH _ _ _ _ xs <- e = mapM_ traverse xs
        | AppEH _ a b <- e = mapM_ traverse [a, b]
        | CaseEH _ x _ y n <- e = mapM_ traverse [x, y, n]
        | otherwise = return ()

      m = execState (traverse e) Map.empty
  in Map.keysSet (Map.filter (== Multi) m)

convert :: Set.Set ID -> ExpH -> Exp
convert share e =
  let -- Generate the definition for this expression.
      defineM :: ExpH -> State [(ID, Exp)] Exp
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
        | AppEH _ f x <- e = do
            f' <- useM f
            x' <- useM x
            return $ AppE f' x'
        | LamEH _ (Sig nm t) _ f <- e = do
            let s' = identify $ \x -> Sig (nm `nappend` (name (show x))) t
            b <- useM (f (VarEH s'))
            return $ LamE s' b
        | CaseEH _ arg s yes no <- e = do
            arg' <- useM arg
            yes' <- useM yes
            no' <- useM no
            return $ CaseE arg' s yes' no'
        | ErrorEH t s <- e = do
            useM $ appEH (varEH (Sig (name "Prelude.error") (arrowT stringT t))) (stringEH s)
            

      -- Generate the use for this expression.
      -- So, if it's shared, turns into a VarE.
      useM :: ExpH -> State [(ID, Exp)] Exp
      useM e
        | Just id <- getid e
        , Set.member id share = do
            m <- get
            let var = VarE (Sig (nameof id) (typeof e))
            case lookup id m of
                Just _ -> return var
                Nothing -> do
                   v <- defineM e
                   modify $ \m -> (id, v):m
                   return var
        | otherwise = defineM e
    
      (body, bindings) = runState (defineM e) []

      nameof :: ID -> Name
      nameof x = name $ "s~" ++ show x
  in letsE [(Sig (nameof x) (typeof v), v) | (x, v) <- reverse bindings] body

