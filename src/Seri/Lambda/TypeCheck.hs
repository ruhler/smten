
module Seri.Lambda.TypeCheck (typecheck) where

import Data.List(nub)

import Seri.Lambda.Env
import Seri.Lambda.IR
import Seri.Lambda.Ppr
import Seri.Lambda.Types


type TypeEnv = [(String, Type)]

-- Type check a flattened seri program.
-- fails if there is an error.
typecheck :: (Monad m) => [Dec] -> m ()
typecheck ds = 
  let checkdec :: (Monad m) => Dec -> m ()
      checkdec (ValD (Sig n t) e) = do
          checkexp [] e
          if (typeof e /= t)
            then fail $ "expecting type " ++ pretty t ++ " in expression "
                        ++ pretty e ++ " but found type " ++ pretty (typeof e)
            else return ()
      checkdec d = error $ "TODO: checkdec " ++ pretty d

      checkpat :: (Monad m) => Pat -> m [(Name, Type)]
      checkpat p@(ConP s@(Sig _ ct) ps) = do
         texpected <- lookupcontype s
         if isSubType texpected ct
            then return ()
            else fail $ "expecting type " ++ pretty texpected ++ ", but found type " ++ pretty ct
         binds <- mapM checkpat ps
         let concated = concat binds
         if length concated /= length (nub (map fst concated))
            then fail $ "VarP appears multiple times in " ++ pretty p
            else return ()
         let twants = init (unarrowsT ct)
         let assertpat w p =
                if w == typeof p
                    then return () 
                    else fail $ "expected type " ++ pretty w ++ " but found type " ++ pretty (typeof p) ++ " in pattern " ++ pretty p
         sequence [assertpat w p | (w, p) <- zip twants ps]
         return concated
      checkpat (VarP (Sig n t)) = return [(n, t)]
      checkpat (IntegerP i) = return []
      checkpat (WildP t) = return []
            

      checkmatch :: (Monad m) => TypeEnv -> Match -> m ()
      checkmatch tenv (Match p b) = do
        bindings <- checkpat p
        checkexp (bindings ++ tenv) b

      -- look up the type of the given data constructor in the environment.
      lookupcontype :: (Monad m) => Sig -> m Type
      lookupcontype (Sig n ct) = do
         let dt = condt ct
         case dt of
                "Bool" -> return $ ConT "Bool"
                "(,)" -> return $ arrowsT [VarT "a", VarT "b",
                            AppT (AppT (ConT "(,)") (VarT "a")) (VarT "b")]
                _ -> case (lookupDataD (mkenv ds ()) dt) of
                        Nothing -> fail $ "unable to find DataD for " ++ dt
                        Just datad ->
                            case typeofCon datad n of
                                Nothing -> fail $ "unable to find constructor " ++ n ++ " in " ++ pretty datad
                                Just t -> return t

      -- checkexp tenv e
      -- Type check an expression.
      --    tenv - a mapping from bound variable name to type
      --    e - the expression to typecheck
      --  fails if expression does not type check.
      checkexp :: (Monad m) => TypeEnv -> Exp -> m ()
      checkexp _ (IntegerE {}) = return ()
      checkexp _ (PrimE {}) = return ()
      checkexp tenv (CaseE e ms) = do
         checkexp tenv e 
         mapM_ (checkmatch tenv) ms
         let badpattypes = filter (\p -> typeof e /= typeof p) [p | Match p _ <- ms]
         if null badpattypes
            then return ()
            else fail $ "Expected type " ++ pretty (typeof e)
                        ++ " in pattern " ++ pretty (head (badpattypes))
                        ++ " but found type " ++ pretty (typeof (head (badpattypes)))
         let badmtypes = [e | typeof e /= typeof (head ms), Match _ e <- ms]
         if null badmtypes
            then return ()
            else fail $ "Expected type " ++ pretty (typeof e)
                        ++ " in match expression " ++ pretty (head (badmtypes))
                        ++ " but found type " ++ pretty (typeof (head (badmtypes)))
      checkexp tenv (AppE f x) = do    
         checkexp tenv f
         checkexp tenv x
         case typeof f of
            (AppT (AppT (ConT "->") a) _) ->
                if a == typeof x
                    then return ()
                    else fail $ "expected type " ++ pretty a ++
                        " but got type " ++ pretty (typeof x) ++
                        " in expression " ++ pretty x
            t -> fail $ "expected function type, but got type " ++ pretty t ++ " in expression " ++ pretty f
      checkexp tenv (LamE (Sig n t) e) = checkexp ((n, t):tenv) e
      checkexp _ c@(ConE s@(Sig n ct)) = do
         texpected <- lookupcontype s
         if isSubType texpected ct
            then return ()
            else fail $ "expecting type " ++ pretty texpected ++ ", but found type " ++ pretty ct
      checkexp tenv (VarE (Sig n t) Bound) =
         case lookup n tenv of
             Just t' | t == t' -> return ()
             Just t' -> fail $ "expected variable of type " ++ pretty t'
                        ++ " but " ++ n ++ " has type " ++ pretty t
             Nothing -> fail $ "unknown bound variable " ++ n
      checkexp tenv v@(VarE (Sig n t) Declared) = error $ "TODO: checkexp Declared var: " ++ pretty v
      checkexp tenv v@(VarE (Sig n t) (Instance _)) = error $ "TODO: checkexp Instance var: " ++ pretty v

  in mapM_ checkdec ds

-- Given the type of a data constructor, return the name of its type
-- constructor.
condt :: Type -> Name
condt (AppT (AppT (ConT "->") _) t) = condt t
condt (AppT a b) = condt a
condt (ConT n) = n

