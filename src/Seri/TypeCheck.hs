
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
typecheck (BoolE _) = return BoolT
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

typecheck (SubE a b) = do
    at <- typecheck a
    typeassert IntegerT at a

    bt <- typecheck b
    typeassert IntegerT bt b

    return IntegerT

typecheck (LtE a b) = do
    at <- typecheck a
    typeassert IntegerT at a

    bt <- typecheck b
    typeassert IntegerT bt b

    return BoolT

typecheck e@(IfE t p a b) = do
    pt <- typecheck p
    typeassert BoolT pt p

    at <- typecheck a
    bt <- typecheck b
    typeassert at bt b
    
    return at

typecheck e@(AppE t f x) = do
    ft <- typecheck f
    xt <- typecheck x
    case ft of
        ArrowT at bt -> do
            typeassert at xt x
            typeassert bt t e
            return bt
        _ -> typefail "function" ft f

typecheck (FixE t n body) = do
    bodyt <- typecheck body
    checkvars n t body
    typeassert t bodyt body
    return t

typecheck (LamE t@(ArrowT at bt) n body) = do
    bodyt <- typecheck body
    checkvars n at body
    typeassert bt bodyt body
    return t

typecheck e@(LamE t n body) = typefail "function" t e
typecheck (VarE t _) = return t

-- Check that all variables with the given name in the expression have the
-- given type. If they don't, fail with a hopefully meaningful message.
checkvars :: Monad m => Name -> Type -> Exp -> m ()
checkvars n v = traverse $ Traversal {
    tr_bool = \_ _ -> return (),
    tr_int = \_ _ -> return (),
    tr_add = \_ a b -> a >> b,
    tr_mul = \_ a b -> a >> b,
    tr_sub = \_ a b -> a >> b,
    tr_lt = \_ a b -> a >> b,
    tr_if = \_ _ p a b -> p >> a >> b,
    tr_app = \_ _ a b -> a >> b,
    tr_fix = \_ _ ln b -> if ln /= n then b else return (),
    tr_lam = \_ _ ln b -> if ln /= n then b else return (),
    tr_var = \e t vn -> if vn == n then typeassert v t e else return ()
}

-- typefail expected found expr
-- Indicate a type failure.
typefail :: (Monad m) => String -> Type -> Exp -> m a
typefail exp fnd expr
  = fail $ "Expected type " ++ exp ++ ", found type " ++ show (ppr fnd)
            ++ " in the expression " ++ show (ppr expr)

typeassert :: (Monad m) => Type -> Type -> Exp -> m ()
typeassert exp fnd expr
  = if (exp == fnd)
        then return ()
        else typefail (show $ ppr exp) fnd expr
  

