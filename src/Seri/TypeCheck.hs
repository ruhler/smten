
module Seri.TypeCheck (
    typecheck
    ) where

import Seri.IR

import Control.Monad.State

-- Typecheck an expression.
-- All the type information should already be in the expression, this just
-- checks that that information is consistent.
--
-- It fails with a hopefully useful message if typecheck fails. Otherwise it
-- returns the type of the expression.
typecheck :: (Monad m) => Exp -> m Type
typecheck (IntegerE _) = return IntegerT

typecheck (AddE a b) = do
    at <- typecheck a
    typeassert IntegerT at a

    bt <- typecheck b
    typeassert IntegerT bt b

    return IntegerT

typecheck (MulE a b) = do
    at <- typecheck a
    typeassert IntegerT at a

    bt <- typecheck b
    typeassert IntegerT bt b

    return IntegerT

typecheck (AppE t f x) = do
    ft <- typecheck f
    xt <- typecheck x
    case ft of
        ArrowT at bt -> do
            typeassert at xt x
            return bt
        _ -> typefail "function" ft f
typecheck (LamE t@(ArrowT at bt) n body) = do
    bodyt <- typecheck body
    checkvars body n at
    typeassert bt bodyt body
    return t

typecheck e@(LamE t n body) = typefail "function" t e
typecheck (VarE t _) = return t

-- Check that all variables with the given name in the expression have the
-- given type. If they don't, fail with a hopefully meaningful message.
checkvars :: Monad m => Exp -> Name -> Type -> m ()
checkvars (IntegerE _) _ _ = return ()
checkvars (AddE a b) n v = do
    checkvars a n v
    checkvars b n v
checkvars (MulE a b) n v = do
    checkvars a n v
    checkvars b n v
checkvars (AppE _ a b) n v = do
    checkvars a n v
    checkvars b n v
checkvars (LamE _ ln body) n v | ln /= n = checkvars body n v
checkvars (LamE _ ln body) n v = return ()
checkvars e@(VarE t vn) n v | vn == n = typeassert v t e
checkvars e@(VarE t vn) n v = return ()


-- typefail expected found expr
-- Indicate a type failure.
typefail :: (Monad m) => String -> Type -> Exp -> m a
typefail exp fnd expr
  = fail $ "Expected type " ++ exp ++ ", found type " ++ show fnd
            ++ " in the expression " ++ show (ppr expr)

typeassert :: (Monad m) => Type -> Type -> Exp -> m ()
typeassert exp fnd expr
  = if (exp == fnd)
        then return ()
        else typefail (show $ ppr exp) fnd expr
  

