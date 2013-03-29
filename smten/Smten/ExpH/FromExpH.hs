
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
fromExpH e = {-# SCC "FromExpH" #-} convert ({-# SCC "SHARING" #-} sharing e) e

data Use = Multi | Single
    deriving (Eq)

-- Find all the subexpressions in the given expression which should be shared.
--
-- It is assumed the expression has been fully pruned, so it is okay to
-- traverse it entirely.
sharing :: ExpH -> Set.Set EID
sharing e =
  let traverse :: ExpH -> State (Map.Map EID Use) (Set.Set EID)
      traverse e
        | issimple e = return Set.empty
        | otherwise = do
             let id = eid e
             m <- get
             case Map.lookup id m of
                Nothing -> do
                     put (Map.insert id Single m)
                     subtraverse e
                Just Single -> do    
                     put (Map.insert id Multi m)
                     return $! Set.singleton id
                Just Multi -> return Set.empty

      subtraverse :: ExpH -> State (Map.Map EID Use) (Set.Set EID)
      subtraverse e
        | ConEH _ _ xs <- force e = Set.unions <$!> mapM traverse xs
        | PrimEH _ _ _ xs <- force e = Set.unions <$!> mapM traverse xs
        | IfEH _ x y n <- force e = Set.unions <$!> mapM traverse [x, y, n]
        | otherwise = return $ Set.empty
  in evalState (traverse e) Map.empty

issimple :: ExpH -> Bool
issimple e =
  case force e of
     LitEH {} -> True
     ConEH _ _ [] -> True
     ConEH {} -> False
     VarEH {} -> True
     PrimEH {} -> False
     LamEH {} -> False
     IfEH {} -> False
     ThunkEH {} -> error "issimple: unexpected ThunkEH"
     ErrorEH {} -> False

data Defined = Defined {
    df_defs :: [(EID, Exp)],
    df_done :: Set.Set EID,
    df_id :: Integer
}

convert :: Set.Set EID -> ExpH -> Exp
convert share e =
  let -- Generate the definition for this expression.
      defineM :: ExpH -> State Defined Exp
      defineM e
        | LitEH l <- force e = return $ LitE l
        | ConEH n t xs <- force e = do
            xs' <- mapM useM xs
            let t' = arrowsT $ (map typeof xs') ++ [t]
            return $ appsE (ConE (Sig n t')) xs'
        | VarEH s <- force e = return $ VarE s
        | PrimEH n t _ xs <- force e = do
            xs' <- mapM useM xs
            let t' = arrowsT $ (map typeof xs') ++ [t]
            return $ appsE (varE (Sig n t')) xs'
        | LamEH (Sig nm t) _ f <- force e = do
            x <- gets df_id
            modifyS $ \df -> df { df_id = x + 1 }
            let s' = Sig (nm `nappend` (name ("~c" ++ show x))) t
            b <- useM (f (exph $ VarEH s'))
            return $ LamE s' b
        | IfEH _ arg yes no <- force e = do
            arg' <- useM arg
            yes' <- useM yes
            no' <- useM no
            return $ ifE arg' yes' no'
        | ErrorEH t s <- force e = return $
            appE (varE (Sig (name "Prelude.error") (arrowT stringT t))) (stringE s)

      -- Generate the use for this expression.
      -- So, if it's shared, turns into a VarE.
      useM :: ExpH -> State Defined Exp
      useM e
        | let id = eid e
        , Set.member id share = do
            done <- gets df_done
            let var = VarE (Sig (nameof id) (typeof (force e)))
            case Set.member id done of
                True -> return var
                False -> do
                   v <- defineM e
                   modifyS $ \df -> df {
                      df_defs = (id, v) : df_defs df,
                      df_done = Set.insert id (df_done df) }
                   return var
        | otherwise = defineM e
    
      (body, defined) = runState (defineM e) (Defined [] Set.empty 0)
      bindings = reverse (df_defs defined)

      nameof :: EID -> Name
      nameof x = name $ "s~" ++ show x
  in letsE [(Sig (nameof x) (typeof v), v) | (x, v) <- bindings] body

