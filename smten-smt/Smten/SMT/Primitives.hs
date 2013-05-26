
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Smten.SMT.Primitives (
    return_symbolicP, fail_symbolicP, bind_symbolicP, nobind_symbolicP,
    return_smtP, fail_smtP, bind_smtP, nobind_smtP,
    __prim_free_IntegerP, __prim_free_BoolP, __prim_free_BitP,
    assertP, usedP, query_UsedP, nestP, useP,
    runSMTP, liftIO_SMTP,
    ) where

import Control.Monad.IO.Class

import Data.Functor((<$>))
import Data.Maybe

import Smten.Type
import Smten.Name
import Smten.Lit
import Smten.ExpH
import Smten.Ppr hiding (nest)
import Smten.SMT.SMT
import qualified Smten.SMT.Solver
import Smten.SMT.Debug
import Smten.SMT.DebugLL
import Smten.SMT.Yices.Yices1
import Smten.SMT.Yices.Yices2
import Smten.SMT.STP.STP
import Smten.Prim

data Solver = Yices1 | Yices2 | STP
            | Debug FilePath Solver
            | DebugLL FilePath Solver
    deriving (Eq, Show)

mksolver :: Solver -> IO Smten.SMT.Solver.Solver
mksolver Yices1 = yices1
mksolver Yices2 = yices2
mksolver STP = stp
mksolver (Debug f s) = do
    s' <- mksolver s
    debug f s'
mksolver (DebugLL f s) = do
    s' <- mksolver s
    debugll f s'

derive_SmtenT "Smten.SMT.Symbolic" ''Solver
derive_SmtenEH "Smten.SMT.Symbolic" ''Solver

derive_SmtenT "Smten.SMT.Symbolic" ''Symbolic
derive_SmtenT "Smten.SMT.Symbolic" ''Used
derive_SmtenT "Smten.SMT.Symbolic" ''SMT

symbolicEH :: Symbolic ExpH -> ExpH
symbolicEH x = litEH (smtenT x) (dynamicL x)

instance (SmtenEH a) => SmtenEH (Symbolic a) where
    smtenEH x = symbolicEH (smtenEH <$> x)
    de_smtenEH e = do
        let q = de_symbolicEH e
        return $ fromMaybe (error "de_smtenEH Symbolic") . de_smtenEH <$> q


usedEH :: Used ExpH -> ExpH
usedEH x = litEH (smtenT x) (dynamicL x)

de_usedEH :: ExpH -> Maybe (Used ExpH)
de_usedEH e = de_litEH e >>= de_dynamicL

instance (SmtenEH a) => SmtenEH (Used a) where
    smtenEH x = usedEH (smtenEH <$> x)
    de_smtenEH e = do
        q <- de_usedEH e
        return $ fromMaybe (error "de_smtenEH Used") . de_smtenEH <$> q

smtEH :: SMT ExpH -> ExpH
smtEH x = litEH (smtenT x) (dynamicL x)

de_smtEH :: ExpH -> Maybe (SMT ExpH)
de_smtEH e = de_litEH e >>= de_dynamicL

instance (SmtenEH a) => SmtenEH (SMT a) where
    smtenEH x = smtEH (smtenEH <$> x)
    de_smtenEH e = do
        q <- de_smtEH e
        return $ fromMaybe (error "de_smtenEH SMT") . de_smtenEH <$> q

return_smtP :: Prim
return_smtP = unaryP "Smten.SMT.Symbolic.return_smt" (return :: ExpH -> SMT ExpH)

fail_smtP :: Prim
fail_smtP = unaryP "Smten.SMT.Symbolic.fail_smt" (fail :: String -> SMT ExpH)

bind_smtP :: Prim
bind_smtP = binaryP "Smten.SMT.Symbolic.bind_smt" ((>>=) :: SMT ExpH -> (ExpH -> SMT ExpH) -> SMT ExpH)

nobind_smtP :: Prim
nobind_smtP = binaryP "Smten.SMT.Symbolic.nobind_smt" ((>>) :: SMT ExpH -> SMT ExpH -> SMT ExpH)

return_symbolicP :: Prim
return_symbolicP = unaryP "Smten.SMT.Symbolic.return_symbolic" (return :: ExpH -> Symbolic ExpH)

fail_symbolicP :: Prim
fail_symbolicP = unaryP "Smten.SMT.Symbolic.fail_symbolic" (fail :: String -> Symbolic ExpH)

bind_symbolicP :: Prim
bind_symbolicP = binaryP "Smten.SMT.Symbolic.bind_symbolic" ((>>=) :: Symbolic ExpH -> (ExpH -> Symbolic ExpH) -> Symbolic ExpH)

nobind_symbolicP :: Prim
nobind_symbolicP = binaryP "Smten.SMT.Symbolic.nobind_symbolic" ((>>) :: Symbolic ExpH -> Symbolic ExpH -> Symbolic ExpH)

free_helper :: Type -> Symbolic ExpH
free_helper t 
  | Just (_, t') <- de_appT t = prim_free t'
  | otherwise = error $ "free_helper: " ++ pretty t
    
__prim_free_IntegerP :: Prim
__prim_free_IntegerP = nullaryTP "Smten.SMT.Symbolic.__prim_free_Integer" free_helper

__prim_free_BoolP :: Prim
__prim_free_BoolP = nullaryTP "Smten.SMT.Symbolic.__prim_free_Bool" free_helper

__prim_free_BitP :: Prim
__prim_free_BitP = nullaryTP "Smten.SMT.Symbolic.__prim_free_Bit" free_helper

assertP :: Prim
assertP = unaryP "Smten.SMT.Symbolic.assert" assert

query_UsedP :: Prim
query_UsedP =
  -- Note: we can't use smtenEH to figure out the return type, because it
  -- doesn't know it. We have to construct the return object based on the
  -- dynamic input type.
  let f :: Used ExpH -> SMT ExpH
      f arg@(Used _ v) = do
        let ta = AppT (ConT maybeN (ArrowK StarK StarK)) (typeof v)
        res <- query_Used (realize <$> arg)
        return $ case res of
                    Just v' -> conEH ta justN [v']
                    Nothing -> conEH ta nothingN []
  in unaryP "Smten.SMT.Symbolic.query_Used" f

usedP :: Prim
usedP = unaryP "Smten.SMT.Symbolic.used" (used :: Used ExpH -> Symbolic ExpH)

useP :: Prim
useP = unaryP "Smten.SMT.Symbolic.use" (use :: Symbolic ExpH -> SMT (Used ExpH))

nestP :: Prim
nestP = unaryP "Smten.SMT.Symbolic.nest" (nest :: SMT ExpH -> SMT ExpH)

runSMTP :: Prim
runSMTP =
  let f :: Solver -> SMT ExpH -> IO ExpH
      f solver q = do
        s <- mksolver solver
        runSMT s q
  in binaryP "Smten.SMT.Symbolic.runSMT" f

liftIO_SMTP :: Prim
liftIO_SMTP = unaryP "Smten.SMT.Symbolic.liftIO_SMT" (liftIO :: IO ExpH -> SMT ExpH)
    

