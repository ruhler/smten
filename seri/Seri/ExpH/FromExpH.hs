
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
fromExpH (LitEH l) = LitE l
fromExpH (ConEH _ n t xs) = 
  let xs' = map fromExpH xs
      t' = arrowsT $ (map typeof xs') ++ [t]
  in appsE (ConE (Sig n t')) xs'
fromExpH (VarEH s) = VarE s
fromExpH (PrimEH _ n t _ xs) =
  let xs' = map fromExpH xs
      t' = arrowsT $ (map typeof xs') ++ [t]
  in appsE (VarE (Sig n t')) xs'
fromExpH (AppEH _ f x) =
  let f' = fromExpH f
      x' = fromExpH x   
  in AppE f' x'
fromExpH (LamEH _ (Sig nm t) _ f) =
  let s' = identify $ \x -> Sig (nm `nappend` (name (show x))) t
      b = fromExpH (f (VarEH s'))
  in LamE s' b
fromExpH (CaseEH _ arg s yes no) =
  let arg' = fromExpH arg
      yes' = fromExpH yes
      no' = fromExpH no
  in CaseE arg' s yes' no'
fromExpH (ErrorEH t s)
  = fromExpH $ appEH (varEH (Sig (name "Prelude.error") (arrowT stringT t))) (stringEH s) 

data Use = Multi | Single
    deriving (Eq)

-- Find all the subexpressions in the given expression which should be shared.
sharing :: ExpH -> Set.Set ID
sharing e =
  let -- Return the ID of the given complex expression, or None if the
      -- expression is simple
      getid :: ExpH -> Maybe ID
      getid e
        | ConEH _ _ _ [] <- e = Nothing
        | ConEH x _ _ _ <- e = Just x
        | PrimEH x _ _ _ _ <- e = Just x
        | AppEH x _ _ <- e = Just x
        | LamEH x _ _ _ <- e = Just x
        | CaseEH x _ _ _ _ <- e = Just x
        | otherwise = Nothing
     
      traverse :: ExpH -> State (Map.Map ID Use) ()
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

